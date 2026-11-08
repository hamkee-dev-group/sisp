#!/usr/bin/env python3
"""Create or verify a complete, portable SISP LLM checkpoint manifest."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
from pathlib import Path


PART = re.compile(r"part-[0-9]+-(?:data|m|v)[.]lsp")
REQUIRED = {"state.lsp", "metrics.txt"}
MANIFEST = "integrity.json"


def checkpoint_files(directory: Path) -> list[Path]:
    if not directory.is_dir():
        raise ValueError(f"not a checkpoint directory: {directory}")
    files: list[Path] = []
    for path in directory.iterdir():
        if path.name == MANIFEST:
            continue
        if path.is_symlink() or not path.is_file():
            raise ValueError(f"unexpected checkpoint entry: {path.name}")
        if path.name not in REQUIRED and not PART.fullmatch(path.name):
            raise ValueError(f"unexpected checkpoint file: {path.name}")
        files.append(path)
    names = {path.name for path in files}
    missing = REQUIRED - names
    if missing:
        raise ValueError(f"checkpoint is missing: {', '.join(sorted(missing))}")
    if not any(PART.fullmatch(name) for name in names):
        raise ValueError("checkpoint contains no parameter parts")
    return sorted(files, key=lambda path: path.name)


def record(path: Path) -> dict[str, object]:
    digest = hashlib.sha256()
    size = 0
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
            size += len(block)
    return {"path": path.name, "bytes": size, "sha256": digest.hexdigest()}


def create(directory: Path) -> None:
    payload = {"format": 1, "files": [record(path) for path in checkpoint_files(directory)]}
    temporary = directory / f"{MANIFEST}.tmp"
    temporary.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(directory / MANIFEST)


def verify(directory: Path) -> None:
    manifest = directory / MANIFEST
    if manifest.is_symlink() or not manifest.is_file():
        raise ValueError(f"checkpoint has no {MANIFEST}")
    payload = json.loads(manifest.read_text(encoding="utf-8"))
    if payload.get("format") != 1 or not isinstance(payload.get("files"), list):
        raise ValueError("unsupported checkpoint integrity manifest")
    actual = [record(path) for path in checkpoint_files(directory)]
    if payload["files"] != actual:
        raise ValueError("checkpoint file set, size, or SHA-256 does not match integrity manifest")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=("create", "verify"))
    parser.add_argument("directory", type=Path)
    args = parser.parse_args()
    try:
        (create if args.mode == "create" else verify)(args.directory)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"checkpoint-integrity: {error}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

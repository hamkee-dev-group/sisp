#!/usr/bin/env python3
"""Run every SISP source in an isolated repository copy.

This is deliberately separate from the regression suite.  A zero exit status
means that a source is load-compatible with the current interpreter; it does
not assert that a definitions-only module is a standalone application.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import os
from pathlib import Path
import re
import subprocess
import sys
import time


ERROR_LINE = re.compile(
    r"(?:ERROR|UNDEFINED|NOT FOUND|MALFORMED|PARSE|ASSERT|ABORT|FAILED)",
    re.IGNORECASE,
)


def source_kind(path: Path) -> str:
    parts = path.parts
    if parts[0] == "lib":
        return "lib-test" if len(parts) > 1 and parts[1] == "tests" else "library"
    if parts[0] == "tests":
        return "test"
    if parts[0] == "ml":
        return "ml"
    if parts[0] == "research":
        return "research"
    if parts[0] == "algorithms":
        return "algorithm"
    if parts[0] == "examples":
        return "example"
    if path.as_posix() == "src/prelude.lsp":
        return "prelude"
    return "other"


def diagnostic(output: str) -> str:
    raised = re.findall(r";\s*([^;\n]+)", output)
    for line in reversed(raised):
        if ERROR_LINE.search(line):
            return line.strip()[:240]
    lines = [line.strip() for line in output.splitlines() if line.strip()]
    for line in reversed(lines):
        if ERROR_LINE.search(line):
            return line[:240]
    if lines:
        return lines[-1][:240]
    return "no diagnostic output"


def run_source(root: Path, binary: Path, source: Path, timeout: float) -> dict[str, object]:
    started = time.monotonic()
    try:
        completed = subprocess.run(
            [str(binary), source.as_posix()],
            cwd=root,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            errors="replace",
            timeout=timeout,
            env={**os.environ, "LC_ALL": "C"},
        )
        state = "ok" if completed.returncode == 0 else "error"
        output = completed.stdout
        status = completed.returncode
    except subprocess.TimeoutExpired as exc:
        state = "timeout"
        captured = exc.stdout or ""
        output = captured.decode(errors="replace") if isinstance(captured, bytes) else captured
        status = None
    return {
        "path": source.as_posix(),
        "kind": source_kind(source),
        "state": state,
        "status": status,
        "seconds": time.monotonic() - started,
        "diagnostic": "" if state == "ok" else diagnostic(output),
    }


def print_report(results: list[dict[str, object]], timeout: float) -> None:
    kinds = sorted({str(result["kind"]) for result in results})
    print("# SISP corpus compatibility report")
    print()
    print(f"Each source was invoked from the repository root with a {timeout:g}-second limit.")
    print("Run this audit in a disposable copy: some example programs write files.")
    print()
    print("| Kind | OK | Error | Timeout | Total |")
    print("|---|---:|---:|---:|---:|")
    for kind in kinds:
        matching = [result for result in results if result["kind"] == kind]
        counts = {state: sum(result["state"] == state for result in matching)
                  for state in ("ok", "error", "timeout")}
        print(f"| {kind} | {counts['ok']} | {counts['error']} | "
              f"{counts['timeout']} | {len(matching)} |")
    print()
    for state, title in (("error", "Errors"), ("timeout", "Timeouts")):
        matching = [result for result in results if result["state"] == state]
        print(f"## {title} ({len(matching)})")
        print()
        if not matching:
            print("None.")
        else:
            for result in matching:
                detail = str(result["diagnostic"]).replace("|", "\\|")
                print(f"- `{result['path']}` — {detail}")
        print()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--binary", type=Path)
    parser.add_argument("--timeout", type=float, default=5.0)
    parser.add_argument("--jobs", type=int, default=min(8, os.cpu_count() or 1))
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    root = args.root.resolve()
    binary = (args.binary or root / "src" / "sisp").resolve()
    if not binary.is_file():
        print(f"missing SISP binary: {binary}", file=sys.stderr)
        return 2
    sources = sorted(path.relative_to(root) for path in root.rglob("*.lsp"))
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
        futures = [executor.submit(run_source, root, binary, source, args.timeout)
                   for source in sources]
        results = [future.result() for future in futures]
    results.sort(key=lambda result: str(result["path"]))
    print_report(results, args.timeout)
    return 1 if any(result["state"] != "ok" for result in results) else 0


if __name__ == "__main__":
    raise SystemExit(main())

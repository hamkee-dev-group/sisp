#!/usr/bin/env python3
"""Prepare reproducible train/validation/test shards for the SISP LLM.

Each input file remains an indivisible unit. Files are normalized, optionally
comment-filtered, deduplicated by content, split, and only then concatenated.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import random
import re
import sys
import unicodedata
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


SMART_PUNCTUATION = str.maketrans({
    "\u2018": "'", "\u2019": "'", "\u201c": '"', "\u201d": '"',
    "\u2013": "-", "\u2014": "-", "\u00a0": " ", "\u2026": "...",
})
ASCII_SYMBOLS = str.maketrans({
    "×": "x", "λ": "lambda", "σ": "sigma", "τ": "tau", "Ω": "Omega",
    "←": "<-", "→": "->", "⟹": "==>", "⟺": "<=>",
    "∃": "exists", "∅": "empty-set", "∈": "in", "∘": "compose",
    "∧": "and", "∩": "intersection", "∪": "union",
    "≠": "/=", "≡": "==", "≤": "<=", "⊆": "subset",
    "⋈": "join", "△": "symmetric-difference",
})
DECORATIVE_COMMENT = re.compile(r"^[;\s*#=+_\-/\\|.]{8,}$")
BOILERPLATE_COMMENT = re.compile(
    r"\b(copyright|all rights reserved|generated (?:file|by)|license|warranty)\b",
    re.IGNORECASE,
)


@dataclass(frozen=True)
class Unit:
    path: str
    text: str
    digest: str
    source_bytes: int
    source_digest: str
    family: str
    source_class: str
    generator: str
    transformations: tuple[str, ...]


def arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", nargs="+", type=Path, help="files or directories")
    parser.add_argument("--output", type=Path, required=True, help="output directory")
    parser.add_argument("--vocab", type=Path, default=Path(__file__).with_name("vocab-code.txt"))
    parser.add_argument("--suffix", action="append", default=[".lsp"], help="accepted suffix; repeatable")
    parser.add_argument("--comments", choices=("keep", "balanced", "strip"), default="balanced")
    parser.add_argument(
        "--unsupported", choices=("reject", "drop"), default="reject",
        help="reject the corpus or drop whole units containing out-of-vocabulary characters",
    )
    parser.add_argument("--seed", type=int, default=20260715)
    parser.add_argument("--train-ratio", type=float, default=0.80)
    parser.add_argument("--valid-ratio", type=float, default=0.10)
    parser.add_argument(
        "--near-duplicate-threshold", type=float, default=0.72,
        help="token-shingle Jaccard threshold used to group related units before splitting",
    )
    parser.add_argument(
        "--fim-rate", type=float, default=0.0,
        help="fraction of training units transformed to prefix/suffix/middle order",
    )
    parser.add_argument("--fim-seed", type=int, default=314159)
    parser.add_argument(
        "--force-train-prefix", action="append", default=[],
        help="repository-relative path prefix whose whole related group must remain in train",
    )
    return parser.parse_args()


def discover(inputs: list[Path], suffixes: set[str]) -> list[Path]:
    paths: set[Path] = set()
    for item in inputs:
        if item.is_file() and item.suffix in suffixes:
            paths.add(item.resolve())
        elif item.is_dir():
            paths.update(
                path.resolve() for path in item.rglob("*")
                if path.is_file() and path.suffix in suffixes
            )
        else:
            raise ValueError(f"not an accepted file or directory: {item}")
    return sorted(paths, key=str)


def keep_balanced_comment(line: str) -> bool:
    body = line.lstrip()[1:].strip()
    if not body or len(body) > 120:
        return False
    if DECORATIVE_COMMENT.fullmatch(body) or BOILERPLATE_COMMENT.search(body):
        return False
    return any(character.isalpha() for character in body)


def string_state_after_line(line: str, in_string: bool) -> bool:
    """Track SISP strings without treating escaped quotes/comments as syntax."""
    index = 0
    while index < len(line):
        character = line[index]
        if character == "\\" and in_string:
            index += 2
            continue
        if character == '"':
            in_string = not in_string
        elif character == ";" and not in_string:
            break
        index += 1
    return in_string


def ascii_compatibility(text: str) -> tuple[str, bool]:
    """Convert Unicode characters with a lossless-looking ASCII decomposition.

    Characters such as ``è`` become ``e``. Symbols with no meaningful ASCII
    decomposition remain untouched so vocabulary enforcement can report them;
    silently deleting arbitrary source characters could change executable code.
    """
    output: list[str] = []
    changed = False
    for character in text:
        if ord(character) < 128:
            output.append(character)
            continue
        decomposed = unicodedata.normalize("NFKD", character)
        replacement = "".join(part for part in decomposed if ord(part) < 128)
        if replacement:
            output.append(replacement)
            changed = True
        else:
            output.append(character)
    return "".join(output), changed


def normalize(raw: str, comments: str) -> tuple[str, tuple[str, ...]]:
    text = unicodedata.normalize("NFKC", raw).translate(SMART_PUNCTUATION)
    output: list[str] = []
    blank = False
    in_string = False
    transformations: set[str] = {"NFKC", "LF-newlines", "tabs-to-two-spaces"}
    for source_line in text.replace("\r\n", "\n").replace("\r", "\n").split("\n"):
        line = source_line.expandtabs(2).rstrip()
        ascii_line = line.translate(ASCII_SYMBOLS)
        if ascii_line != line:
            transformations.add("ascii-symbol-transliteration")
            line = ascii_line
        line, compatibility_changed = ascii_compatibility(line)
        if compatibility_changed:
            transformations.add("ascii-compatibility-transliteration")
        stripped = line.lstrip()
        comment_line = not in_string and stripped.startswith(";")
        if comment_line:
            if comments == "strip" or (comments == "balanced" and not keep_balanced_comment(line)):
                transformations.add("comment-filtering")
                continue
        if not line:
            if blank:
                continue
            blank = True
        else:
            blank = False
        output.append(line)
        if not comment_line:
            in_string = string_state_after_line(line, in_string)
    return "\n".join(output).strip() + "\n", tuple(sorted(transformations))


def unit_metadata(path: str) -> tuple[str, str, str]:
    name = Path(path).stem.lower()
    # Classify inputs by repository directory.
    generator = "unspecified"
    source_class = "repository"
    family = name
    family = re.sub(r"-(?:variant|rewrite|sample|v)-?[0-9]+$", "", family)
    family = re.sub(r"^[0-9]+-", "", family)
    if path.startswith("ml/training/seed/"):
        source_class = "training-seed"
    elif path.startswith("ml/training/gen/"):
        source_class = "generated"
    elif path.startswith("tests/"):
        source_class = "repository-test"
    elif path.startswith("ml/llm/"):
        source_class = "repository-llm"
    elif path.startswith("ml/agent/"):
        source_class = "repository-agent"
    elif path.startswith("examples/"):
        source_class = "repository-example"
    return family, source_class, generator


def load_units(paths: list[Path], comments: str) -> tuple[list[Unit], list[dict[str, str]]]:
    unique: dict[str, Unit] = {}
    duplicates: list[dict[str, str]] = []
    for path in paths:
        raw_bytes = path.read_bytes()
        try:
            raw = raw_bytes.decode("utf-8")
        except UnicodeDecodeError as error:
            raise ValueError(f"{path}: not UTF-8 ({error})") from error
        text, transformations = normalize(raw, comments)
        if not text.strip():
            continue
        digest = hashlib.sha256(text.encode()).hexdigest()
        try:
            display_path = str(path.relative_to(Path.cwd()))
        except ValueError:
            display_path = path.name
        family, source_class, generator = unit_metadata(display_path)
        unit = Unit(display_path, text, digest, len(raw_bytes),
                    hashlib.sha256(raw_bytes).hexdigest(), family,
                    source_class, generator, transformations)
        if digest in unique:
            duplicates.append({"path": display_path, "same_as": unique[digest].path, "sha256": digest})
        else:
            unique[digest] = unit
    return list(unique.values()), duplicates


def token_shingles(text: str, width: int = 7) -> set[int]:
    code = "\n".join(line.split(";", 1)[0] for line in text.splitlines())
    tokens = re.findall(r'"[^"\n]*"|[(){}\[\]`\',]|[^\s(){}\[\]`\',]+', code.lower())
    if len(tokens) < width:
        return set()
    return {
        int.from_bytes(hashlib.blake2b("\0".join(tokens[index:index + width]).encode(),
                                      digest_size=8).digest(), "big")
        for index in range(len(tokens) - width + 1)
    }


def near_duplicate_pairs(units: list[Unit], threshold: float) -> list[tuple[int, int, float]]:
    if not 0.0 <= threshold <= 1.0:
        raise ValueError("near-duplicate threshold must be between zero and one")
    shingles = [token_shingles(unit.text) for unit in units]
    prime = 18446744073709551557
    signatures: list[tuple[int, ...]] = []
    for values in shingles:
        if not values:
            signatures.append(tuple())
            continue
        # Sixteen four-hash bands give a ~99.3% candidate probability at the
        # default 0.72 threshold, followed by exact Jaccard confirmation.
        signatures.append(tuple(
            min(((value * (2 * offset + 1)) + (offset * 0x9E3779B97F4A7C15)) % prime
                for value in values)
            for offset in range(64)
        ))
    buckets: dict[tuple[int, tuple[int, ...]], list[int]] = defaultdict(list)
    for index, signature in enumerate(signatures):
        if not signature:
            continue
        for band in range(16):
            buckets[(band, signature[band * 4:(band + 1) * 4])].append(index)
    candidates: set[tuple[int, int]] = set()
    for members in buckets.values():
        for left_index in range(len(members)):
            for right_index in range(left_index + 1, len(members)):
                candidates.add((members[left_index], members[right_index]))
    pairs: list[tuple[int, int, float]] = []
    for left, right in sorted(candidates):
        union = shingles[left] | shingles[right]
        if not union:
            continue
        similarity = len(shingles[left] & shingles[right]) / len(union)
        if similarity >= threshold:
            pairs.append((left, right, similarity))
    return pairs


def split_units(units: list[Unit], near_pairs: list[tuple[int, int, float]], seed: int,
                train_ratio: float, valid_ratio: float,
                force_train_prefixes: tuple[str, ...] = ()) -> dict[str, list[Unit]]:
    if not 0.0 < train_ratio < 1.0 or not 0.0 <= valid_ratio < 1.0 or train_ratio + valid_ratio >= 1.0:
        raise ValueError("ratios must leave non-zero probability for the test split")
    parents = list(range(len(units)))

    def find(index: int) -> int:
        while parents[index] != index:
            parents[index] = parents[parents[index]]
            index = parents[index]
        return index

    def union(left: int, right: int) -> None:
        left_root, right_root = find(left), find(right)
        if left_root != right_root:
            parents[right_root] = left_root

    family_owner: dict[str, int] = {}
    for index, unit in enumerate(units):
        if unit.family in family_owner:
            union(index, family_owner[unit.family])
        else:
            family_owner[unit.family] = index
    for left, right, _ in near_pairs:
        union(left, right)
    grouped: dict[int, list[Unit]] = defaultdict(list)
    for index, unit in enumerate(units):
        grouped[find(index)].append(unit)
    ordered = sorted(grouped.values(), key=lambda group: min(unit.digest for unit in group))
    random.Random(seed).shuffle(ordered)
    ordered.sort(key=lambda group: sum(len(unit.text.encode()) for unit in group), reverse=True)
    total_bytes = sum(len(unit.text.encode()) for group in ordered for unit in group)
    train_target = total_bytes * train_ratio
    valid_target = total_bytes * valid_ratio
    train_groups: list[list[Unit]] = []
    valid_groups: list[list[Unit]] = []
    test_groups: list[list[Unit]] = []
    target_bytes = {
        "train": train_target,
        "valid": valid_target,
        "test": total_bytes - train_target - valid_target,
    }
    assigned_bytes = {"train": 0, "valid": 0, "test": 0}
    assigned_groups = {
        "train": train_groups,
        "valid": valid_groups,
        "test": test_groups,
    }
    forced_groups = [group for group in ordered if any(
        unit.path.startswith(prefix) for unit in group for prefix in force_train_prefixes
    )]
    for group in forced_groups:
        train_groups.append(group)
        assigned_bytes["train"] += sum(len(unit.text.encode()) for unit in group)
    for group in ordered:
        if group in forced_groups:
            continue
        size = sum(len(unit.text.encode()) for unit in group)
        destination = max(
            ("train", "valid", "test"),
            key=lambda name: ((target_bytes[name] - assigned_bytes[name]) / target_bytes[name],
                              target_bytes[name]),
        )
        assigned_groups[destination].append(group)
        assigned_bytes[destination] += size
    if len(ordered) >= 3:
        if not valid_groups:
            valid_groups.append(train_groups.pop())
        if not test_groups:
            test_groups.append(valid_groups.pop() if len(valid_groups) > 1 else train_groups.pop())
    train = [unit for group in train_groups for unit in group]
    valid = [unit for group in valid_groups for unit in group]
    test = [unit for group in test_groups for unit in group]
    return {
        "train": train,
        "valid": valid,
        "test": test,
    }


def enforce_vocabulary(units: list[Unit], vocabulary: str, mode: str) -> tuple[list[Unit], list[dict[str, object]]]:
    accepted: list[Unit] = []
    excluded: list[dict[str, object]] = []
    allowed = set(vocabulary)
    for unit in units:
        bad = sorted(set(unit.text) - allowed)
        if bad:
            excluded.append({
                "path": unit.path,
                "sha256": unit.digest,
                "unsupported": [f"U+{ord(character):04X}" for character in bad],
            })
        else:
            accepted.append(unit)
    if excluded and mode == "reject":
        characters = sorted({code for row in excluded for code in row["unsupported"]})
        raise ValueError(
            f"{len(excluded)} units contain characters outside the frozen vocabulary: "
            + ", ".join(characters)
        )
    return accepted, excluded


def write_span_file(path: Path, variable: str, spans: list[tuple[int, int]]) -> None:
    rows = " ".join(f"[{start} {length} 0]" for start, length in spans)
    path.write_text(f"(setq {variable} [{rows}])\n", encoding="ascii")


def fim_text(unit: Unit, args: argparse.Namespace) -> tuple[str, bool]:
    if not 0.0 <= args.fim_rate <= 1.0:
        raise ValueError("FIM rate must be between zero and one")
    if args.fim_rate == 0.0:
        return unit.text.rstrip(), False
    if any(unit.path.startswith(prefix)
           for prefix in getattr(args, "force_train_prefix", [])):
        return unit.text.rstrip(), False
    chooser = random.Random(f"{args.fim_seed}:{unit.digest}")
    lines = unit.text.rstrip().splitlines(keepends=True)
    if len(lines) < 4 or chooser.random() >= args.fim_rate:
        return unit.text.rstrip(), False
    middle_start = chooser.randrange(1, len(lines) - 1)
    middle_end = chooser.randrange(middle_start + 1, len(lines))
    prefix = "".join(lines[:middle_start])
    middle = "".join(lines[middle_start:middle_end])
    suffix = "".join(lines[middle_end:])
    transformed = (
        "; <fim-prefix>\n" + prefix
        + "; <fim-suffix>\n" + suffix
        + "\n; <fim-middle>\n" + middle
    )
    return transformed.rstrip(), True


def write_outputs(output: Path, splits: dict[str, list[Unit]], duplicates: list[dict[str, str]],
                  near_pairs: list[tuple[int, int, float]], units: list[Unit],
                  excluded: list[dict[str, object]], vocabulary: str,
                  args: argparse.Namespace) -> None:
    output.mkdir(parents=True, exist_ok=True)
    manifest: list[dict[str, object]] = []
    report: dict[str, object] = {
        "seed": args.seed,
        "comments": args.comments,
        "vocabulary_size": len(vocabulary),
        "duplicates": duplicates,
        "near_duplicate_threshold": args.near_duplicate_threshold,
        "near_duplicates": [
            {"left": units[left].path, "right": units[right].path,
             "similarity": round(similarity, 6)}
            for left, right, similarity in near_pairs[:1000]
        ],
        "near_duplicates_truncated": max(0, len(near_pairs) - 1000),
        "fim_rate": args.fim_rate,
        "fim_seed": args.fim_seed,
        "force_train_prefixes": getattr(args, "force_train_prefix", []),
        "excluded": excluded,
        "splits": {},
    }
    source_classes: dict[str, dict[str, int]] = defaultdict(lambda: {"units": 0, "bytes": 0})
    family_splits: dict[str, set[str]] = defaultdict(set)
    for split_name, split_units_list in splits.items():
        for unit in split_units_list:
            source_classes[unit.source_class]["units"] += 1
            source_classes[unit.source_class]["bytes"] += len(unit.text.encode())
            family_splits[unit.family].add(split_name)
    leaked = {family: sorted(names) for family, names in family_splits.items() if len(names) > 1}
    if leaked:
        raise ValueError(f"family groups leaked across splits: {leaked}")
    report["source_classes"] = dict(sorted(source_classes.items()))
    report["families"] = len(family_splits)
    boundary = "\n; --- sisp-corpus-unit ---\n"
    for name, units in splits.items():
        pieces: list[str] = []
        spans: list[tuple[int, int]] = []
        emitted: dict[str, tuple[int, bool]] = {}
        position = 0
        for index, unit in enumerate(units):
            if index:
                pieces.append(boundary)
                position += len(boundary)
            text, fim_transformed = (
                fim_text(unit, args) if name == "train" else (unit.text.rstrip(), False)
            )
            pieces.append(text)
            spans.append((position, len(text)))
            emitted[unit.digest] = (len(text.encode()), fim_transformed)
            position += len(text)
        corpus = "".join(pieces) + ("\n" if units else "")
        bad = sorted(set(corpus) - set(vocabulary))
        if bad:
            rendered = ", ".join(f"U+{ord(char):04X} {char!r}" for char in bad)
            raise ValueError(f"{name} contains characters outside the frozen vocabulary: {rendered}")
        encoded = corpus.encode()
        (output / f"{name}.txt").write_text(corpus, encoding="utf-8")
        variable = {
            "train": "*matrix-training-spans*",
            "valid": "*matrix-validation-spans*",
            "test": "*matrix-test-spans*",
        }[name]
        write_span_file(output / f"{name}-spans.lsp", variable, spans)
        report["splits"][name] = {
            "units": len(units), "bytes": len(encoded), "sha256": hashlib.sha256(encoded).hexdigest(),
        }
        manifest.extend({
            "unit_id": unit.digest[:16], "split": name, "path": unit.path,
            "origin": unit.path, "sha256": unit.digest, "family_id": unit.family,
            "source_sha256": unit.source_digest,
            "source_class": unit.source_class, "generator": unit.generator,
            "license": "SISP-repository-policy",
            "source_bytes": unit.source_bytes, "normalized_bytes": len(unit.text.encode()),
            "corpus_bytes": emitted[unit.digest][0],
            "fim_transformed": emitted[unit.digest][1],
            "forced_train": any(unit.path.startswith(prefix)
                                for prefix in getattr(args, "force_train_prefix", [])),
            "validation": ("load-and-oracle" if "ml/training/" in unit.path
                           else "repository-source"),
            "transformations": list(unit.transformations),
        } for unit in units)
    with (output / "manifest.jsonl").open("w", encoding="utf-8", newline="\n") as stream:
        for row in manifest:
            stream.write(json.dumps(row, sort_keys=True) + "\n")
    (output / "report.json").write_text(
        json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )


def main() -> int:
    args = arguments()
    try:
        vocabulary = args.vocab.read_text(encoding="utf-8")
        if len(vocabulary) != len(set(vocabulary)):
            raise ValueError("vocabulary contains duplicate characters")
        paths = discover(args.inputs, set(args.suffix))
        if not paths:
            raise ValueError("no matching source files found")
        units, duplicates = load_units(paths, args.comments)
        units, excluded = enforce_vocabulary(units, vocabulary, args.unsupported)
        if len(units) < 3:
            raise ValueError("at least three distinct normalized units are required")
        near_pairs = near_duplicate_pairs(units, args.near_duplicate_threshold)
        splits = split_units(units, near_pairs, args.seed, args.train_ratio, args.valid_ratio,
                            tuple(args.force_train_prefix))
        write_outputs(args.output, splits, duplicates, near_pairs, units, excluded,
                      vocabulary, args)
    except (OSError, ValueError) as error:
        print(f"prepare-corpus: {error}", file=sys.stderr)
        return 2
    print(json.dumps({name: len(units) for name, units in splits.items()}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

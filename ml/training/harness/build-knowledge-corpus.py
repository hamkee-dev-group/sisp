#!/usr/bin/env python3
"""Build a source-grounded SISP knowledge corpus.

The executable examples are compact teachers, not bulk text.  This program
turns their explicit structure, comments, definitions, call relationships, and
verified demonstrations into short documentation records.  Interpreter source
and prelude definitions are treated as the language authority.

Generated text is ASCII and every record is at most 255 bytes so a complete
prompt/answer can fit the 256-character student context.  Output lives under
ml/llm/data/local and is intentionally ignored by git.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import unicodedata
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
DEFAULT_OUTPUT = ROOT / "ml/llm/data/local/sisp-knowledge-v1"
MAX_RECORD_BYTES = 224
DEFAULT_TARGET = 24 * 1024 * 1024

IDENTIFIER = re.compile(r"[A-Za-z0-9_#*+/<=>\\^-]+")
DEFINE_FUNCTION = re.compile(
    r"^\(define\s+\(([A-Za-z0-9_#*+/<=>\\^-]+)([^)]*)\)", re.S
)
DEFINE_VALUE = re.compile(r"^\(define\s+([A-Za-z0-9_#*+/<=>\\^-]+)\s", re.S)
PRINT_STRING = re.compile(r'\(print\s+"([^"\n]+)"\)')
BUILTIN_ENTRY = re.compile(r'\{"([^"\n]+)",\s*(builtin_[A-Za-z0-9_]+),')

STOP_CALLS = {
    "define", "lambda", "let", "let*", "labels", "cond", "if", "progn",
    "quote", "bquote", "setq", "and", "or", "not", "t", "nil",
}


@dataclass(frozen=True)
class Record:
    source: str
    topic: str
    kind: str
    text: str


def ascii_text(value: str) -> str:
    value = value.replace("×", " times ").replace("→", " to ")
    value = value.replace("≤", " <= ").replace("≥", " >= ").replace("≠", " != ")
    value = unicodedata.normalize("NFKD", value).encode("ascii", "ignore").decode("ascii")
    value = value.replace("\t", " ").replace("\r", " ")
    return value


def compact(value: str) -> str:
    return re.sub(r"\s+", " ", ascii_text(value)).strip()


def short_source(path: Path) -> str:
    rel = path.relative_to(ROOT).as_posix()
    for suffix in (".lsp", ".c", ".h", ".md"):
        if rel.endswith(suffix):
            rel = rel[: -len(suffix)]
            break
    return rel.replace("examples/", "")


def topic_for(path: Path, text: str) -> str:
    comments = []
    for line in text.splitlines()[:24]:
        stripped = line.strip()
        if stripped.startswith(";"):
            value = compact(stripped.lstrip(";= -"))
            if value and len(value) > 3 and not value.lower().startswith(("unique:", "file:")):
                comments.append(value)
        elif stripped and path.suffix == ".lsp":
            break
    if comments:
        candidate = comments[0]
        if len(candidate) <= 72:
            return candidate
    return compact(path.stem.replace("_", " ").replace("-", " "))


def truncate(value: str, budget: int) -> str:
    value = compact(value)
    if len(value.encode("ascii")) <= budget:
        return value
    clipped = value.encode("ascii")[: max(0, budget - 3)].decode("ascii", "ignore")
    if " " in clipped:
        clipped = clipped.rsplit(" ", 1)[0]
    return clipped.rstrip(" ,;:") + "..."


def short_label(value: str, budget: int) -> str:
    """Shorten metadata at a word boundary without creating a false phrase."""
    value = compact(value)
    if len(value.encode("ascii")) <= budget:
        return value
    clipped = value.encode("ascii")[:budget].decode("ascii", "ignore")
    if " " in clipped:
        clipped = clipped.rsplit(" ", 1)[0]
    return clipped.rstrip(" ,;:")


def make_record(source: str, topic: str, kind: str, lines: list[str]) -> Record | None:
    clean = [compact(line) for line in lines if compact(line)]
    if not clean:
        return None
    text = "\n".join(clean)
    if len(text.encode("ascii")) > MAX_RECORD_BYTES:
        return None
    return Record(source, topic, kind, text)


class Corpus:
    def __init__(self) -> None:
        self.records: dict[str, Record] = {}
        self.atomic_facts: defaultdict[str, list[tuple[str, str, str]]] = defaultdict(list)
        self.atomic_keys: defaultdict[str, set[str]] = defaultdict(set)

    def add(self, record: Record | None) -> None:
        if record is None:
            return
        if any(ord(ch) > 127 for ch in record.text):
            raise ValueError(f"non-ASCII record from {record.source}")
        self.records.setdefault(record.text, record)

    def qa(self, source: str, topic: str, kind: str, question: str, answer: str,
           remember: bool = True) -> None:
        original_topic = compact(topic)
        source = short_label(source, 55)
        topic = short_label(original_topic, 48)
        question = compact(question)
        answer = compact(answer)
        if original_topic != topic:
            question = question.replace(original_topic, topic)
        if not question or not answer:
            return

        # Keep the factual pair available for source-local relationship lessons.
        key = f"{question}\0{answer}"
        if remember and key not in self.atomic_keys[source]:
            self.atomic_keys[source].add(key)
            self.atomic_facts[source].append((question, answer, topic))

        layouts = [
            ("qa", [f"; source: {source}; topic: {topic}", f"; question: {question}", f"; answer: {answer}"]),
            ("qa-brief", [f"; SISP study: {topic}", f"; Q: {question}", f"; A: {answer}"]),
            ("qa-prompt", [f"; prompt: {question}", f"; response: {answer}", f"; evidence: {source}"]),
            ("qa-explain", [f"; explain {topic}: {question}", f"; explanation: {answer}", f"; reference: {source}"]),
            ("qa-dialogue", [f"; learner asks: {question}", f"; SISP answers: {answer}", f"; source: {source}"]),
            ("qa-manual", [f"; documentation question: {question}", f"; documented answer: {answer}", f"; from: {source}"]),
        ]
        for suffix, lines in layouts:
            # Never cut an answer mid-thought merely to fit the context.  A
            # compact layout may fit when a verbose one does not.
            self.add(make_record(source, topic, f"{kind}-{suffix}", lines))

    def fact(self, source: str, topic: str, kind: str, fact: str) -> None:
        fact = compact(fact)
        if len(fact) < 8:
            return
        self.add(make_record(short_label(source, 55), short_label(topic, 48),
                             f"{kind}-fact", [
                                 f"; source: {short_label(source, 55)}; topic: {short_label(topic, 48)}",
                                 f"; fact: {fact}",
                             ]))
        self.qa(source, topic, kind, f"What does the source teach about {topic}?", fact)

    def code(self, source: str, topic: str, name: str, form: str) -> None:
        form = compact(form)
        header = f"; source: {source}; definition: {name}"
        if len((header + "\n" + form).encode("ascii")) <= MAX_RECORD_BYTES:
            self.add(make_record(source, topic, "code-complete", [header, form]))
        # Source excerpts remain SISP comments, so truncated forms cannot become
        # misleading executable definitions.
        excerpt_budget = MAX_RECORD_BYTES - len(f"; source code for {name} in {source}: ")
        excerpt = truncate(form, max(32, excerpt_budget))
        self.add(make_record(source, topic, "code-excerpt", [
            f"; source code for {name} in {source}: {excerpt}"
        ]))


def source_paths() -> tuple[list[Path], list[Path]]:
    lsp: list[Path] = [ROOT / "src/prelude.lsp"]
    lsp.extend(sorted((ROOT / "tests").rglob("*.lsp")))
    for path in sorted((ROOT / "examples").rglob("*.lsp")):
        rel = path.relative_to(ROOT / "examples")
        if rel.parts[0] == "training":
            continue
        if rel.parts[0] == "llm" and len(rel.parts) > 1 and rel.parts[1] in {
            "checkpoints", "generated"
        }:
            continue
        if rel.parts[:3] == ("llm", "data", "local"):
            continue
        lsp.append(path)

    docs = [ROOT / "README.md"]
    docs.extend(sorted((ROOT / "examples").rglob("README.md")))
    docs.extend(sorted((ROOT / "ml/llm").glob("*.md")))
    docs = sorted(set(path for path in docs if path.exists()))
    return sorted(set(lsp)), docs


def top_forms(text: str) -> list[tuple[int, str]]:
    forms: list[tuple[int, str]] = []
    start = None
    depth = 0
    in_string = False
    in_comment = False
    index = 0
    while index < len(text):
        ch = text[index]
        if in_comment:
            if ch == "\n":
                in_comment = False
            index += 1
            continue
        if in_string:
            if ch == '"':
                in_string = False
            index += 1
            continue
        if ch == ";":
            in_comment = True
        elif ch == '"':
            in_string = True
        elif ch == "(":
            if depth == 0:
                start = index
            depth += 1
        elif ch == ")" and depth:
            depth -= 1
            if depth == 0 and start is not None:
                forms.append((start, text[start:index + 1]))
                start = None
        index += 1
    return forms


def clean_form(form: str) -> str:
    lines = []
    for line in form.splitlines():
        before = line.split(";", 1)[0]
        if before.strip():
            lines.append(before)
    return compact(" ".join(lines))


def preceding_comment(text: str, position: int) -> str:
    lines = text[:position].splitlines()
    block = []
    for line in reversed(lines):
        stripped = line.strip()
        if stripped.startswith(";"):
            value = compact(stripped.lstrip(";= -"))
            if value:
                block.append(value)
        elif not stripped:
            if block:
                break
        else:
            break
    return compact(" ".join(reversed(block)))


def calls_in(form: str, own_name: str) -> list[str]:
    clean = re.sub(r'"[^"\n]*"', " ", clean_form(form))
    calls = []
    for match in re.finditer(r"\(([A-Za-z0-9_#*+/<=>\\^-]+)", clean):
        name = match.group(1)
        if name not in STOP_CALLS and name != own_name and name not in calls:
            calls.append(name)
    return calls


def comment_sentences(text: str) -> list[str]:
    blocks: list[str] = []
    current: list[str] = []
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith(";"):
            value = compact(stripped.lstrip(";= -"))
            if value and not re.fullmatch(r"[-= ]+", value):
                current.append(value)
        elif current:
            blocks.append(" ".join(current))
            current = []
    if current:
        blocks.append(" ".join(current))

    values = []
    for block in blocks:
        sentences = re.split(r"(?<=[.!?])\s+", compact(block))
        for sentence in sentences:
            value = compact(sentence).strip("-= ")
            if (8 <= len(value) <= 220
                    and not value.lower().startswith(("=>", "should produce"))
                    and not value.lower().endswith(" done")
                    and "=== done ===" not in value.lower()):
                values.append(value)
    return values


def useful_display_text(value: str) -> bool:
    value = compact(value).strip("-= ")
    lowered = value.lower()
    if len(value) < 10:
        return False
    if lowered.endswith(" done") or lowered in {"done", "output", "result", "demo"}:
        return False
    if lowered.startswith(("begin ", "end ")):
        return False
    return True


def prose_sentences(text: str) -> list[str]:
    text = re.sub(r"```.*?```", " ", text, flags=re.S)
    text = re.sub(r"\[([^]]+)\]\([^)]+\)", r"\1", text)
    blocks: list[str] = []
    current: list[str] = []
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            if current:
                blocks.append(" ".join(current))
                current = []
            continue
        if line.startswith(("#", "- ", "* ", "|")):
            if current:
                blocks.append(" ".join(current))
                current = []
            blocks.append(line)
        else:
            current.append(line)
    if current:
        blocks.append(" ".join(current))

    values = []
    for block in blocks:
        block = re.sub(r"[#|`*_>]", " ", block)
        for chunk in re.split(r"(?<=[.!?])\s+", block):
            value = compact(chunk.lstrip("-0123456789. "))
            value = re.sub(r"\s+([.,;:!?])", r"\1", value)
            if 18 <= len(value) <= 220 and not value.startswith(("http", "src/sisp")):
                values.append(value)
    return values


def process_lsp(corpus: Corpus, path: Path) -> dict[str, object]:
    raw = path.read_text(errors="replace")
    text = ascii_text(raw)
    source = short_source(path)
    topic = topic_for(path, text)
    definitions: list[tuple[str, list[str], str]] = []

    corpus.qa(source, topic, "source-identity",
              f"What topic does {source} demonstrate?", topic)

    for sentence in comment_sentences(text):
        corpus.fact(source, topic, "source-comment", sentence)

    for printed in PRINT_STRING.findall(text):
        value = compact(printed).strip("-= ")
        if len(value) <= 180 and useful_display_text(value):
            corpus.fact(source, topic, "demonstration-text", value)

    for position, raw_form in top_forms(text):
        form = clean_form(raw_form)
        match = DEFINE_FUNCTION.match(form)
        value_match = DEFINE_VALUE.match(form) if match is None else None
        if match:
            name = match.group(1)
            parameters = IDENTIFIER.findall(match.group(2))
            calls = calls_in(form, name)
            definitions.append((name, calls, form))
            signature = f"({name}{(' ' + ' '.join(parameters)) if parameters else ''})"
            corpus.qa(source, topic, "signature",
                      f"What is the SISP signature of {name}?", signature)
            corpus.qa(source, topic, "definition-location",
                      f"Where is {name} implemented?", f"In {source}, which teaches {topic}.")
            purpose = preceding_comment(text, position)
            if purpose:
                corpus.qa(source, topic, "definition-purpose",
                          f"What source comment introduces {name}?", purpose)
            if parameters:
                corpus.qa(source, topic, "parameters",
                          f"Which parameters does {name} receive?", ", ".join(parameters) + ".")
                for number, parameter in enumerate(parameters, 1):
                    corpus.qa(source, topic, "parameter-position",
                              f"What is parameter {number} of {name}?", parameter)
            if calls:
                corpus.qa(source, topic, "dependencies",
                          f"Which operations does {name} call directly?", ", ".join(calls) + ".")
                for called in calls:
                    corpus.qa(source, topic, "call-edge",
                              f"Does {name} directly use {called}?",
                              f"Yes. The definition of {name} contains a call to {called}.")
            else:
                corpus.qa(source, topic, "dependencies",
                          f"Does {name} call another named operation?",
                          "No direct operation call is visible in its definition.")
            corpus.code(source, topic, name, form)
        elif value_match:
            name = value_match.group(1)
            definitions.append((name, calls_in(form, name), form))
            corpus.qa(source, topic, "binding",
                      f"What kind of SISP name is {name} in {source}?",
                      f"It is a top-level value defined for the {topic} example.")
            corpus.code(source, topic, name, form)
        elif (len(form) <= 190
              and form.startswith(("(assert", "(print", "(check", "(pass"))
              and not re.fullmatch(r'\(print\s+"[^"\n]*"\)', form)):
            corpus.qa(source, topic, "demonstration-form",
                      f"What executable form appears in the {topic} demonstration?", form)

    # Source-local relationships teach system structure without inventing
    # semantics.  Pairing is capped per file but broad enough for large projects.
    pair_count = 0
    for left_index, (left, left_calls, _) in enumerate(definitions):
        for right, right_calls, _ in definitions[left_index + 1:]:
            answer = f"Both {left} and {right} are defined in {source}."
            if right in left_calls:
                answer += f" {left} directly calls {right}."
            elif left in right_calls:
                answer += f" {right} directly calls {left}."
            corpus.qa(source, topic, "definition-relation",
                      f"How are {left} and {right} related in this SISP source?", answer)
            pair_count += 1
            if pair_count >= 500:
                break
        if pair_count >= 500:
            break

    return {"source": source, "topic": topic, "definitions": len(definitions), "bytes": len(raw)}


def process_doc(corpus: Corpus, path: Path) -> dict[str, object]:
    raw = path.read_text(errors="replace")
    text = ascii_text(raw)
    source = short_source(path)
    topic = compact(path.parent.name.replace("-", " ") + " documentation")
    sentences = prose_sentences(text)
    corpus.qa(source, topic, "documentation-identity",
              f"What does {source} document?", topic)
    for sentence in sentences:
        corpus.fact(source, topic, "documentation", sentence)
    for left, right in zip(sentences, sentences[1:]):
        corpus.qa(source, topic, "documentation-sequence",
                  f"Which two documented ideas occur together in {source}?",
                  f"{left} {right}")
    return {"source": source, "topic": topic, "sentences": len(sentences), "bytes": len(raw)}


def find_c_handler(handler: str, c_paths: list[Path]) -> tuple[Path | None, str]:
    pattern = re.compile(r"\b" + re.escape(handler) + r"\s*\([^;]*?\)\s*\{", re.S)
    for path in c_paths:
        text = path.read_text(errors="replace")
        match = pattern.search(text)
        if not match:
            continue
        start = text.find("{", match.start())
        depth = 0
        in_string = False
        index = start
        while index < len(text):
            ch = text[index]
            if ch == '"':
                in_string = not in_string
            elif not in_string and ch == "{":
                depth += 1
            elif not in_string and ch == "}":
                depth -= 1
                if depth == 0:
                    return path, text[start:index + 1]
            index += 1
    return None, ""


def process_c_source(corpus: Corpus) -> dict[str, object]:
    c_paths = sorted((ROOT / "src").glob("*.c"))
    builtins_text = (ROOT / "src/builtins.c").read_text(errors="replace")
    entries = BUILTIN_ENTRY.findall(builtins_text)
    aliases: defaultdict[str, list[str]] = defaultdict(list)
    for name, handler in entries:
        aliases[handler].append(name)

    located = 0
    for name, handler in entries:
        path, body = find_c_handler(handler, c_paths)
        if path is None:
            source = "src/builtins"
            category = "interpreter builtin"
        else:
            located += 1
            source = short_source(path)
            category = {
                "special": "special form and evaluator control",
                "builtins-matrix": "native matrix operation",
                "builtins-set": "set theory and relations",
                "builtins-math": "numeric and algebraic operation",
            }.get(path.stem, "interpreter builtin")
        topic = f"SISP builtin {name}"
        corpus.qa(source, topic, "builtin-handler",
                  f"Which C handler implements the SISP name {name}?", handler)
        corpus.qa(source, topic, "builtin-category",
                  f"Which interpreter area contains {name}?", category + ".")
        if path is not None:
            corpus.qa(source, topic, "builtin-source",
                      f"Where is the implementation of {name}?", source + ".")
        names = aliases[handler]
        if len(names) > 1:
            corpus.qa(source, topic, "builtin-alias",
                      f"Which SISP names share handler {handler}?", ", ".join(names) + ".")
        if body:
            calls = []
            for called in re.findall(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*\(", body):
                if called not in {handler, "if", "while", "for", "switch", "sizeof"} and called not in calls:
                    calls.append(called)
            if calls:
                corpus.qa(source, topic, "builtin-internals",
                          f"Which C operations are visible inside {handler}?", ", ".join(calls[:12]) + ".")
                for called in calls[:12]:
                    corpus.qa(source, topic, "builtin-call-edge",
                              f"Does handler {handler} use {called}?",
                              f"Yes. The authoritative C body contains a call to {called}.")
            for expression in re.findall(r"\breturn\s+([^;]{1,180});", body):
                expression = compact(expression)
                corpus.qa(source, topic, "builtin-return",
                          f"What return expression is visible in {handler}?", expression)
            for assertion in re.findall(r"_ASSERTP\s*\(([^;]{1,180})\);", body, flags=re.S):
                assertion = compact(assertion)
                corpus.qa(source, topic, "builtin-contract",
                          f"Which checked contract is visible in {handler}?", assertion)

    # C comments explain evaluator, parser, memory, matrices, and numerical
    # contracts.  Preserve them as source-grounded lessons.
    for path in c_paths + sorted((ROOT / "src").glob("*.h")):
        text = ascii_text(path.read_text(errors="replace"))
        source = short_source(path)
        topic = path.stem.replace("-", " ") + " implementation"
        comments = re.findall(r"/\*(.*?)\*/|//([^\n]*)", text, flags=re.S)
        for block, line in comments:
            value = compact(block or line)
            for sentence in re.split(r"(?<=[.!?])\s+|\n+", value):
                sentence = compact(sentence.lstrip("* "))
                if 12 <= len(sentence) <= 220:
                    corpus.fact(source, topic, "c-source-comment", sentence)

    return {"builtins": len(entries), "located_handlers": located, "c_files": len(c_paths)}


def add_cross_fact_lessons(corpus: Corpus) -> None:
    # Combine two independently source-grounded statements only within the same
    # source.  The wording claims co-location, not an invented causal relation.
    for source, facts in list(corpus.atomic_facts.items()):
        if len(facts) < 2:
            continue
        limit = min(len(facts), 180)
        selected = sorted(facts, key=lambda item: hashlib.sha256("\0".join(item).encode()).digest())[:limit]
        pairs = 0
        for offset in range(1, min(18, len(selected))):
            for index, (_, left_answer, left_topic) in enumerate(selected):
                _, right_answer, right_topic = selected[(index + offset) % len(selected)]
                if left_answer == right_answer:
                    continue
                corpus.qa(source, left_topic, "source-synthesis",
                          f"Which two facts are documented together in {source}?",
                          f"{left_answer} Also, {right_answer}", remember=False)
                corpus.qa(source, right_topic, "source-review",
                          f"Review two source-grounded ideas from {source}.",
                          f"First: {left_answer} Second: {right_answer}", remember=False)
                pairs += 1
                if pairs >= 120:
                    break
            if pairs >= 120:
                break


def stable_value(text: str) -> int:
    return int.from_bytes(hashlib.sha256(text.encode("ascii")).digest()[:8], "big")


def select_records(records: list[Record], target: int) -> list[Record]:
    # Preserve semantic teaching before using source-local relationships to fill
    # the requested scale.  Otherwise large projects with many definition pairs
    # would crowd out their own comments, demonstrations, and code.
    semantic_prefixes = (
        "code-", "source-comment-", "demonstration-", "documentation-",
        "c-source-comment-", "definition-purpose-", "builtin-", "source-identity-",
    )
    structural_prefixes = (
        "signature-", "parameters-", "parameter-position-", "dependencies-",
        "call-edge-", "definition-location-", "binding-",
    )

    def priority(record: Record) -> int:
        if record.kind.startswith(semantic_prefixes):
            return 0
        if record.kind.startswith(structural_prefixes):
            return 1
        if record.kind.startswith("definition-relation-"):
            return 2
        return 3

    ordered = sorted(records, key=lambda rec: (
        priority(rec), stable_value("sisp-knowledge-v1\0" + rec.text)
    ))
    selected = []
    total = 0
    for record in ordered:
        selected.append(record)
        total += len(record.text.encode("ascii")) + 2
        if total >= target:
            break
    return selected


def write_split(path: Path, records: list[Record]) -> int:
    data = "\n\n".join(record.text for record in records) + "\n"
    path.write_text(data, encoding="ascii")
    return len(data.encode("ascii"))


def write_span_file(path: Path, split: str, size: int) -> None:
    variable = {
        "train": "*matrix-training-spans*",
        "valid": "*matrix-validation-spans*",
        "test": "*matrix-test-spans*",
    }[split]
    path.write_text(f"(setq {variable} [[0 {size} 0]])\n", encoding="ascii")


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--target-bytes", type=int, default=DEFAULT_TARGET,
                        help="total bytes across train/valid/test (default: 24 MiB)")
    args = parser.parse_args()

    corpus = Corpus()
    lsp_paths, doc_paths = source_paths()
    inventory = [process_lsp(corpus, path) for path in lsp_paths]
    documentation = [process_doc(corpus, path) for path in doc_paths]
    c_summary = process_c_source(corpus)
    add_cross_fact_lessons(corpus)

    candidates = list(corpus.records.values())
    selected = select_records(candidates, args.target_bytes)
    selected_bytes = sum(len(record.text.encode("ascii")) + 2 for record in selected)
    if selected_bytes < args.target_bytes:
        raise SystemExit(
            f"only {selected_bytes} unique bytes available; target is {args.target_bytes}"
        )

    splits: dict[str, list[Record]] = {"train": [], "valid": [], "test": []}
    source_splits: dict[str, str] = {}
    for record in selected:
        # All paraphrases, code, and relationships derived from one source stay
        # together.  Interpreter source, prelude, and tests are language truth,
        # so they always belong to training; held-out examples measure transfer.
        if record.source.startswith(("src/", "tests/")):
            split = "train"
        else:
            bucket = stable_value("source-split\0" + record.source) % 100
            split = "train" if bucket < 90 else "valid" if bucket < 95 else "test"
        source_splits[record.source] = split
        splits[split].append(record)
    for records in splits.values():
        records.sort(key=lambda rec: stable_value("order\0" + rec.text))

    args.output.mkdir(parents=True, exist_ok=True)
    sizes = {name: write_split(args.output / f"{name}.txt", records)
             for name, records in splits.items()}
    for name, size in sizes.items():
        write_span_file(args.output / f"{name}-spans.lsp", name, size)
    checksums = {name: file_sha256(args.output / f"{name}.txt") for name in splits}
    span_checksums = {
        name: file_sha256(args.output / f"{name}-spans.lsp") for name in splits
    }
    kinds = Counter(record.kind for record in selected)
    candidate_kinds = Counter(record.kind for record in candidates)
    sources = Counter(record.source for record in selected)
    lengths = [len(record.text.encode("ascii")) for record in selected]
    manifest = {
        "name": "sisp-knowledge-v1",
        "contract": {
            "authority": ["src C implementation", "src/prelude.lsp", "tests", "examples"],
            "character_vocabulary": "printable ASCII plus newline",
            "maximum_record_bytes": MAX_RECORD_BYTES,
            "records_are_exactly_deduplicated": True,
            "split": "stable source-file hash; authoritative src/tests forced into train",
        },
        "inputs": {
            "lsp_files": len(lsp_paths),
            "documentation_files": len(doc_paths),
            "lsp_source_bytes": sum(item["bytes"] for item in inventory),
            "documentation_source_bytes": sum(item["bytes"] for item in documentation),
            **c_summary,
        },
        "output": {
            "candidate_records": len(candidates),
            "selected_records": len(selected),
            "selected_sources": len(sources),
            "minimum_record_bytes": min(lengths),
            "maximum_record_bytes": max(lengths),
            "mean_record_bytes": sum(lengths) / len(lengths),
            "split_records": {name: len(records) for name, records in splits.items()},
            "split_bytes": sizes,
            "split_sha256": checksums,
            "span_sha256": span_checksums,
            "split_sources": dict(Counter(source_splits.values())),
            "total_bytes": sum(sizes.values()),
            "record_kinds": dict(sorted(kinds.items())),
            "candidate_record_kinds": dict(sorted(candidate_kinds.items())),
        },
        "source_inventory": inventory,
        "source_splits": dict(sorted(source_splits.items())),
    }
    (args.output / "manifest.json").write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="ascii"
    )
    print(json.dumps({
        "candidate_records": len(candidates),
        "selected_records": len(selected),
        "selected_sources": len(sources),
        "split_records": manifest["output"]["split_records"],
        "split_bytes": sizes,
        "split_sha256": checksums,
        "span_sha256": span_checksums,
        "total_bytes": sum(sizes.values()),
    }, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

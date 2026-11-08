#!/usr/bin/env python3
"""Validate the exact tiny-agent policy corpus and its held-out fixtures."""

from __future__ import annotations

import argparse
import collections
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
TRAIN = ROOT / "ml/agent/protocol/train"
EVAL = ROOT / "ml/agent/eval"
VOCABULARY = ROOT / "ml/llm/vocab-code.txt"
INTERPRETER = ROOT / "src/sisp"
AGENT = ROOT / "ml/agent/agent.lsp"
PROTOCOL = ";A:(agent-eval 'F)|(agent-recall K)|(agent-remember K V)|(agent-think T)|(agent-answer X)"
EXPECTED_TRAIN_KINDS = {
    "eval": 36,
    "answer": 64,
    "remember": 8,
    "recall": 12,
    "think": 8,
}
EXPECTED_EVAL_KINDS = {
    "eval": 7,
    "answer": 12,
    "remember": 1,
    "recall": 2,
    "think": 2,
}


@dataclass(frozen=True)
class Transition:
    source: Path
    number: int
    goal: str
    observation: str
    memory: str
    action: str
    kind: str

    @property
    def prompt(self) -> str:
        return "\n".join((f";G:{self.goal}", f";O:{self.observation}",
                           f";M:{self.memory}", PROTOCOL)) + "\n"


def fail(message: str) -> None:
    raise ValueError(message)


def action_kind(action: str, location: str) -> str:
    for kind in ("eval", "recall", "remember", "think", "answer"):
        if action.startswith(f"(agent-{kind} ") or action == f"(agent-{kind})":
            if action.count("(agent-") != 1:
                fail(f"{location}: action must contain exactly one agent tool call")
            return kind
    fail(f"{location}: action is not one of the five agent tools")
    raise AssertionError


def load_file(path: Path, expected_blocks: int, vocabulary: set[str]) -> list[Transition]:
    raw = path.read_text(encoding="utf-8")
    if "\r" in raw:
        fail(f"{path}: CR newlines are forbidden")
    for number, line in enumerate(raw.splitlines(), 1):
        if line.rstrip() != line:
            fail(f"{path}:{number}: trailing whitespace changes the runtime protocol")
    blocks = [block for block in raw.strip().split("\n\n") if block]
    if len(blocks) != expected_blocks:
        fail(f"{path}: expected {expected_blocks} transitions, found {len(blocks)}")
    transitions: list[Transition] = []
    for index, block in enumerate(blocks, 1):
        lines = block.splitlines()
        location = f"{path}:{index}"
        if len(lines) != 5:
            fail(f"{location}: transition must have exactly five lines")
        if not lines[0].startswith(";G:") or not lines[1].startswith(";O:") or not lines[2].startswith(";M:"):
            fail(f"{location}: transition fields must be G, O, M, A, action")
        if lines[3] != PROTOCOL:
            fail(f"{location}: action menu differs from the runtime protocol")
        goal, observation, memory, action = lines[0][3:], lines[1][3:], lines[2][3:], lines[4]
        if not goal:
            fail(f"{location}: goal is empty")
        for label, value, maximum in (("goal", goal, 70), ("observation", observation, 40),
                                      ("memory", memory, 24)):
            if len(value) > maximum:
                fail(f"{location}: {label} is {len(value)} chars, maximum is {maximum}")
        prompt = "\n".join(lines[:4]) + "\n"
        if len(prompt) > 256:
            fail(f"{location}: prompt is {len(prompt)} chars, maximum is 256")
        if len(action) + 1 > 96:
            fail(f"{location}: action plus terminating newline exceeds 96 chars")
        unsupported = sorted(set(prompt + action + "\n") - vocabulary)
        if unsupported:
            fail(f"{location}: characters outside frozen vocabulary: {unsupported!r}")
        kind = action_kind(action, location)
        transitions.append(Transition(path, index, goal, observation, memory, action, kind))
    return transitions


def validate_episodes(files: list[Path], blocks: int, vocabulary: set[str]) -> list[Transition]:
    transitions: list[Transition] = []
    for path in files:
        current = load_file(path, blocks, vocabulary)
        for offset in range(0, len(current), 2):
            first, second = current[offset], current[offset + 1]
            if first.goal != second.goal:
                fail(f"{path}: the two turns of episode {offset // 2 + 1} have different goals")
            if second.kind != "answer":
                fail(f"{path}: episode {offset // 2 + 1} must finish with agent-answer")
            if first.kind == "answer":
                fail(f"{path}: episode {offset // 2 + 1} answers before observing a tool result")
        transitions.extend(current)
    return transitions


def sisp_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def execute_episode(first: Transition, second: Transition, workspace: Path) -> None:
    stem = f"{first.source.stem}-{first.number}"
    memory = workspace / f"{stem}.memory"
    transcript = workspace / f"{stem}.transcript"
    config = workspace / f"{stem}.config.lsp"
    case = workspace / f"{stem}.case.lsp"
    memory.write_text((first.memory + "\n") if first.memory else "", encoding="ascii")
    transcript.write_text("", encoding="ascii")
    config.write_text("\n".join((
        f'(define *matrix-vocabulary* (str-to-list (read-file {sisp_string(str(VOCABULARY))})))',
        '(define *matrix-context* 256)',
        f'(define *agent-memory-path* {sisp_string(str(memory))})',
        f'(define *agent-transcript-path* {sisp_string(str(transcript))})',
        f'(define *agent-goal-path* {sisp_string(str(workspace / "goal.txt"))})',
        f'(define *agent-action-path* {sisp_string(str(workspace / "action.lsp"))})',
        '(define *agent-max-turns* 2)',
        '(define *agent-action-chars* 96)',
        '(define *agent-temperature* 0.3)',
        '(define *agent-top-k* 1)',
        '(define *agent-top-p* 1.0)',
    )) + "\n", encoding="ascii")
    expected_prefix = second.observation
    case.write_text("\n".join((
        '(agent-memory-refresh)',
        '(setq *agent-tool-count* 0)',
        '(setq *agent-action-recognized* nil)',
        first.action,
        f'(cond ((and *agent-action-recognized* (equal *agent-action-kind* {sisp_string(first.kind)}) '
        f'(= (str-find *agent-observation* {sisp_string(expected_prefix)}) 0)) nil) (t (quit 2)))',
        '(setq *agent-done* nil)',
        '(setq *agent-tool-count* 0)',
        '(setq *agent-action-recognized* nil)',
        second.action,
        '(cond ((and *agent-action-recognized* (equal *agent-action-kind* "answer") '
        '*agent-done* (= (str-find *agent-observation* "answer=>") 0)) nil) (t (quit 2)))',
        '(print "protocol-ok")',
    )) + "\n", encoding="ascii")
    result = subprocess.run(
        [str(INTERPRETER), str(config), str(AGENT), str(case)],
        cwd=ROOT, text=True, capture_output=True, timeout=20, check=False,
    )
    if result.returncode != 0 or result.stderr or '"protocol-ok"' not in result.stdout:
        fail(f"{first.source}: episode {(first.number + 1) // 2} failed in SISP\n"
             f"stdout: {result.stdout}\nstderr: {result.stderr}")
    if first.kind == "remember":
        stored = memory.read_text(encoding="ascii").rstrip("\n")
        if stored != second.memory:
            fail(f"{first.source}: remembered text {stored!r} != next memory {second.memory!r}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--static", action="store_true", help="skip execution through src/sisp")
    args = parser.parse_args()
    vocabulary = set(VOCABULARY.read_text(encoding="ascii"))
    train_files = sorted(TRAIN.glob("*.txt"))
    eval_files = sorted(EVAL.glob("*.txt"))
    if len(train_files) != 8:
        fail(f"expected 8 protocol training files, found {len(train_files)}")
    if len(eval_files) != 12:
        fail(f"expected 12 held-out policy files, found {len(eval_files)}")
    train = validate_episodes(train_files, 16, vocabulary)
    heldout = validate_episodes(eval_files, 2, vocabulary)
    if len(train) != 128 or len(heldout) != 24:
        fail(f"wrong transition totals: train={len(train)} heldout={len(heldout)}")
    train_kinds = collections.Counter(item.kind for item in train)
    eval_kinds = collections.Counter(item.kind for item in heldout)
    if dict(train_kinds) != EXPECTED_TRAIN_KINDS:
        fail(f"wrong training action distribution: {dict(train_kinds)}")
    if dict(eval_kinds) != EXPECTED_EVAL_KINDS:
        fail(f"wrong held-out action distribution: {dict(eval_kinds)}")
    train_pairs = {(item.prompt, item.action) for item in train}
    heldout_pairs = {(item.prompt, item.action) for item in heldout}
    if len(train_pairs) != len(train):
        fail("training data contains an exact prompt/action duplicate")
    if len(heldout_pairs) != len(heldout):
        fail("held-out data contains an exact prompt/action duplicate")
    leaked = train_pairs & heldout_pairs
    if leaked:
        fail(f"held-out prompt/action pairs leaked into training: {len(leaked)}")
    if not args.static:
        if not INTERPRETER.is_file():
            fail("src/sisp is missing; build the interpreter or use --static")
        with tempfile.TemporaryDirectory(prefix="sisp-agent-protocol-") as directory:
            workspace = Path(directory)
            (workspace / "goal.txt").write_text("protocol validation\n", encoding="ascii")
            (workspace / "action.lsp").write_text("", encoding="ascii")
            for transitions in (train, heldout):
                for offset in range(0, len(transitions), 2):
                    execute_episode(transitions[offset], transitions[offset + 1], workspace)
    print(f"agent protocol data: train {len(train) // 2} episodes/{len(train)} targets; "
          f"heldout {len(heldout) // 2} episodes/{len(heldout)} targets; ok")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

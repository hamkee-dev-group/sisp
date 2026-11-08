#!/usr/bin/env python3
"""Prepare held-out agent policy tasks and score their greedy action files."""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
EVAL = ROOT / "ml/agent/eval"
VOCABULARY = ROOT / "ml/llm/vocab-code.txt"
INTERPRETER = ROOT / "src/sisp"
AGENT = ROOT / "ml/agent/agent.lsp"
PROTOCOL = ";A:(agent-eval 'F)|(agent-recall K)|(agent-remember K V)|(agent-think T)|(agent-answer X)"
RESULT = re.compile(r"\(agent-policy-result\s+(t|nil)\s+(t|nil)\s+(t|nil)\)")


def sisp_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def parse_block(block: str, path: Path, number: int) -> dict[str, str]:
    lines = block.splitlines()
    if len(lines) != 5 or lines[3] != PROTOCOL:
        raise ValueError(f"{path}: malformed transition {number}")
    for prefix, line in zip((";G:", ";O:", ";M:"), lines[:3]):
        if not line.startswith(prefix):
            raise ValueError(f"{path}: malformed {prefix} in transition {number}")
    action = lines[4]
    match = re.match(r"^\(agent-(eval|recall|remember|think|answer)(?:\s|\))", action)
    if not match:
        raise ValueError(f"{path}: unknown target action in transition {number}")
    return {
        "goal": lines[0][3:],
        "observation": lines[1][3:],
        "memory": lines[2][3:],
        "prompt": "\n".join(lines[:4]) + "\n",
        "target": action,
        "kind": match.group(1),
    }


def prepare(workspace: Path) -> None:
    workspace.mkdir(parents=True, exist_ok=True)
    prompts = workspace / "prompts"
    targets = workspace / "targets"
    candidates = workspace / "candidates"
    prompts.mkdir()
    targets.mkdir()
    candidates.mkdir()
    cases: list[dict[str, object]] = []
    tasks: list[str] = []
    files = sorted(EVAL.glob("*.txt"))
    if len(files) != 12:
        raise ValueError(f"expected 12 held-out episode files, found {len(files)}")
    for path in files:
        blocks = [block for block in path.read_text(encoding="utf-8").strip().split("\n\n") if block]
        if len(blocks) != 2:
            raise ValueError(f"{path}: expected two transitions")
        parsed = [parse_block(block, path, index + 1) for index, block in enumerate(blocks)]
        if parsed[0]["goal"] != parsed[1]["goal"]:
            raise ValueError(f"{path}: episode goals differ")
        for turn, case in enumerate(parsed, 1):
            name = f"{path.stem}-t{turn}"
            prompt_path = prompts / f"{name}.prompt"
            target_path = targets / f"{name}.target"
            prompt_path.write_text(str(case["prompt"]), encoding="ascii")
            target_path.write_text(str(case["target"]), encoding="ascii")
            if turn == 1:
                expected_observation = str(parsed[1]["observation"])
                expected_done = False
            else:
                answer = re.fullmatch(r'\(agent-answer "([^"\n]*)"\)', str(case["target"]))
                if answer is None:
                    raise ValueError(f"{path}: second transition is not a simple final answer")
                expected_observation = "answer=>" + answer.group(1)
                expected_done = True
            cases.append({
                "name": name,
                "prompt": str(prompt_path),
                "target": str(target_path),
                "candidate": str(candidates / f"{name}.lsp"),
                "kind": case["kind"],
                "memory": case["memory"],
                "expected_observation": expected_observation,
                "expected_done": expected_done,
            })
            tasks.append(
                f"(functional-eval-one {sisp_string(name)} "
                f"{sisp_string(str(prompt_path))} {sisp_string(str(target_path))})"
            )
    if len(cases) != 24:
        raise ValueError(f"expected 24 held-out targets, found {len(cases)}")
    tasks.append("(functional-eval-summary)")
    (workspace / "tasks.lsp").write_text("\n".join(tasks) + "\n", encoding="ascii")
    (workspace / "manifest.json").write_text(
        json.dumps(cases, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    print(f"agent policy evaluation prepared: {len(cases)} targets")


def first_complete_action(generated: str) -> str:
    """Mirror the agent's first-balanced-form stop, with newline fallback."""
    depth = 0
    started = False
    in_string = False
    for index, character in enumerate(generated):
        if in_string:
            if character == '"':
                in_string = False
            continue
        if character == '"':
            in_string = True
        elif character == "(":
            started = True
            depth += 1
        elif character == ")" and started:
            depth -= 1
            if depth == 0:
                return generated[:index + 1]
        elif character == "\n":
            return generated[:index]
    return generated


def execute_candidate(case: dict[str, object], workspace: Path) -> tuple[bool, bool, bool, bool]:
    name = str(case["name"])
    prompt = Path(str(case["prompt"])).read_text(encoding="ascii")
    target = Path(str(case["target"])).read_text(encoding="ascii")
    candidate_path = Path(str(case["candidate"]))
    if not candidate_path.is_file():
        raise FileNotFoundError(f"model evaluator did not create {candidate_path}")
    candidate = candidate_path.read_text(encoding="ascii")
    if not candidate.startswith(prompt):
        raise ValueError(f"candidate lost its prompt prefix: {candidate_path}")
    generated = candidate[len(prompt):]
    generated_action = first_complete_action(generated)
    exact = generated_action.strip() == target.strip()
    case_dir = workspace / "execution" / name
    case_dir.mkdir(parents=True)
    memory = case_dir / "memory.txt"
    memory_text = str(case["memory"])
    memory.write_text((memory_text + "\n") if memory_text else "", encoding="ascii")
    transcript = case_dir / "transcript.txt"
    transcript.write_text("", encoding="ascii")
    goal = case_dir / "goal.txt"
    goal.write_text("held-out policy evaluation\n", encoding="ascii")
    action_slot = case_dir / "action.lsp"
    action_slot.write_text("", encoding="ascii")
    executable = case_dir / "generated-action.lsp"
    executable.write_text(prompt + generated_action + "\n", encoding="ascii")
    config = case_dir / "config.lsp"
    config.write_text("\n".join((
        f'(define *matrix-vocabulary* (str-to-list (read-file {sisp_string(str(VOCABULARY))})))',
        '(define *matrix-context* 256)',
        f'(define *agent-memory-path* {sisp_string(str(memory))})',
        f'(define *agent-transcript-path* {sisp_string(str(transcript))})',
        f'(define *agent-goal-path* {sisp_string(str(goal))})',
        f'(define *agent-action-path* {sisp_string(str(action_slot))})',
        '(define *agent-max-turns* 2)',
        '(define *agent-action-chars* 96)',
        '(define *agent-temperature* 0.3)',
        '(define *agent-top-k* 1)',
        '(define *agent-top-p* 1.0)',
    )) + "\n", encoding="ascii")
    expected_kind = sisp_string(str(case["kind"]))
    expected_observation = sisp_string(str(case["expected_observation"]))
    expected_done = "t" if bool(case["expected_done"]) else "nil"
    check = case_dir / "check.lsp"
    check.write_text(
        "(print (list (quote agent-policy-result)\n"
        "  (and *agent-action-recognized* (= *agent-tool-count* 1))\n"
        f"  (equal *agent-action-kind* {expected_kind})\n"
        f"  (and (= (str-find *agent-observation* {expected_observation}) 0)\n"
        f"       (eq *agent-done* {expected_done}))))\n",
        encoding="ascii",
    )
    try:
        result = subprocess.run(
            [str(INTERPRETER), str(config), str(AGENT), str(executable), str(check)],
            cwd=ROOT, text=True, capture_output=True, timeout=10, check=False,
        )
    except subprocess.TimeoutExpired:
        return exact, False, False, False
    match = RESULT.search(result.stdout)
    parsed = result.returncode == 0 and not result.stderr and match is not None
    if match is None:
        return exact, parsed, False, False
    recognized = match.group(1) == "t" and match.group(2) == "t"
    semantic = recognized and match.group(3) == "t"
    return exact, parsed, recognized, semantic


def score(workspace: Path) -> dict[str, int]:
    manifest = workspace / "manifest.json"
    if not manifest.is_file():
        raise FileNotFoundError(f"missing evaluation manifest: {manifest}")
    cases = json.loads(manifest.read_text(encoding="utf-8"))
    if len(cases) != 24:
        raise ValueError(f"evaluation manifest has {len(cases)} cases, expected 24")
    totals = {"exact": 0, "parsed": 0, "recognized_kind": 0, "semantic": 0}
    for case in cases:
        exact, parsed, recognized, semantic = execute_candidate(case, workspace)
        totals["exact"] += int(exact)
        totals["parsed"] += int(parsed)
        totals["recognized_kind"] += int(recognized)
        totals["semantic"] += int(semantic)
        print(f'(agent-policy-action "{case["name"]}" exact {str(exact).lower()} '
              f'parsed {str(parsed).lower()} recognized-kind {str(recognized).lower()} '
              f'semantic {str(semantic).lower()})')
    print("(agent-policy-action-summary cases 24 "
          f"exact {totals['exact']} parsed {totals['parsed']} "
          f"recognized-kind {totals['recognized_kind']} semantic {totals['semantic']})")
    return totals


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("prepare", "score"))
    parser.add_argument("workspace", type=Path)
    parser.add_argument("--minimum-semantic", type=int, default=0)
    args = parser.parse_args()
    if not 0 <= args.minimum_semantic <= 24:
        parser.error("--minimum-semantic must be between 0 and 24")
    if args.command == "prepare":
        prepare(args.workspace.resolve())
    else:
        totals = score(args.workspace.resolve())
        if totals["semantic"] < args.minimum_semantic:
            print(
                f"agent policy failed: {totals['semantic']} semantic actions; "
                f"required {args.minimum_semantic}",
                file=sys.stderr,
            )
            return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

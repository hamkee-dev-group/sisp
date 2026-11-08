#!/usr/bin/env python3
"""Drive the SISP agent with any real model command.

The command receives the SISP-built prompt on stdin and must write exactly one
SISP agent action on stdout.  It is executed without a shell.  SISP remains the
owner of agent state, validation, tools, memory, budgets, and transcripts.
"""

from __future__ import annotations

import argparse
import json
import select
import shlex
import subprocess
import sys
import time
from pathlib import Path


REPO = Path(__file__).resolve().parents[2]
DEFAULT_WORKSPACE = REPO / "ml/agent/generated/external"


def sisp_string(value: Path) -> str:
    text = str(value)
    if any(character in text for character in ('"', "\\", "\n", "\r")):
        raise SystemExit(f"path cannot be represented as a SISP string: {text!r}")
    return f'"{text}"'


def wait_for(path: Path, expected: str, process: subprocess.Popen[str], timeout: float) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if path.is_file() and path.read_text(encoding="utf-8") == expected:
            return
        status = process.poll()
        if status is not None:
            raise SystemExit(f"SISP exited unexpectedly with status {status}")
        time.sleep(0.01)
    raise SystemExit(f"timed out waiting for SISP synchronization: {expected}")


def send(process: subprocess.Popen[str], form: str) -> None:
    if process.stdin is None:
        raise SystemExit("SISP stdin is unavailable")
    process.stdin.write(form)
    process.stdin.write("\n")
    process.stdin.flush()


def synchronize(
    process: subprocess.Popen[str], sync_path: Path, token: str, timeout: float
) -> None:
    sync_path.unlink(missing_ok=True)
    send(process, f"(write-file {sisp_string(sync_path)} \"{token}\")")
    wait_for(sync_path, token, process, timeout)


def model_completion(command: list[str], prompt: str, timeout: float, maximum: int) -> str:
    try:
        result = subprocess.run(
            command,
            input=prompt,
            text=True,
            capture_output=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired as error:
        raise SystemExit(f"model command exceeded its {timeout:g}-second timeout") from error
    if result.stderr:
        sys.stderr.write(result.stderr)
    if result.returncode != 0:
        raise SystemExit(f"model command exited with status {result.returncode}")
    if result.stdout == "":
        raise SystemExit("model command returned an empty completion")
    if len(result.stdout.encode("utf-8")) > maximum:
        raise SystemExit(
            f"model command returned more than the {maximum}-byte response limit"
        )
    return result.stdout


class PersistentModel:
    """One long-lived model process using one JSON request/response per line."""

    def __init__(self, command: list[str], timeout: float, maximum: int) -> None:
        self.timeout = timeout
        self.maximum = maximum
        self.process = subprocess.Popen(
            command,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=None,
            text=True,
            bufsize=1,
        )

    def complete(self, prompt: str) -> str:
        if self.process.stdin is None or self.process.stdout is None:
            raise SystemExit("persistent model pipes are unavailable")
        request = json.dumps({"prompt": prompt}, ensure_ascii=True)
        self.process.stdin.write(request + "\n")
        self.process.stdin.flush()
        ready, _, _ = select.select([self.process.stdout], [], [], self.timeout)
        if not ready:
            raise SystemExit(
                f"persistent model exceeded its {self.timeout:g}-second timeout"
            )
        line = self.process.stdout.readline()
        if not line:
            status = self.process.poll()
            raise SystemExit(f"persistent model exited unexpectedly with status {status}")
        if len(line.encode("utf-8")) > self.maximum:
            raise SystemExit(
                f"persistent model returned more than the {self.maximum}-byte limit"
            )
        try:
            response = json.loads(line)
        except json.JSONDecodeError as error:
            raise SystemExit("persistent model returned invalid JSON") from error
        if isinstance(response, dict) and isinstance(response.get("error"), str):
            raise SystemExit(f"persistent model error: {response['error']}")
        completion = response.get("completion") if isinstance(response, dict) else None
        if not isinstance(completion, str) or completion == "":
            raise SystemExit("persistent model returned an empty completion")
        return completion

    def close(self) -> None:
        if self.process.stdin is not None:
            try:
                self.process.stdin.close()
            except BrokenPipeError:
                pass
        try:
            self.process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.process.terminate()
            self.process.wait(timeout=5)
        if self.process.stdout is not None:
            self.process.stdout.close()


def configuration(workspace: Path, turns: int, context: int) -> str:
    values = {
        "memory": workspace / "memory.txt",
        "transcript": workspace / "transcript.txt",
        "goal": workspace / "goal.txt",
        "action": workspace / "action.lsp",
        "request": workspace / "request.txt",
        "response": workspace / "response.lsp",
        "status": workspace / "status.txt",
        "final": workspace / "final.txt",
    }
    return "\n".join(
        [
            f"(define *matrix-context* {context})",
            '(define *matrix-vocabulary* (str-to-list (read-file "ml/llm/vocab-code.txt")))',
            f"(define *agent-memory-path* {sisp_string(values['memory'])})",
            f"(define *agent-transcript-path* {sisp_string(values['transcript'])})",
            f"(define *agent-goal-path* {sisp_string(values['goal'])})",
            f"(define *agent-action-path* {sisp_string(values['action'])})",
            f"(define *agent-max-turns* {turns})",
            "(define *agent-action-chars* 0)",
            "(define *agent-temperature* 0.0)",
            "(define *agent-top-k* 1)",
            "(define *agent-top-p* 1.0)",
            f"(define *external-agent-request-path* {sisp_string(values['request'])})",
            f"(define *external-agent-response-path* {sisp_string(values['response'])})",
            f"(define *external-agent-status-path* {sisp_string(values['status'])})",
            f"(define *external-agent-final-path* {sisp_string(values['final'])})",
            "",
        ]
    )


def arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run the SISP agent with a command-line model backend"
    )
    goal = parser.add_mutually_exclusive_group(required=True)
    goal.add_argument("--goal", help="agent goal text")
    goal.add_argument("--goal-file", type=Path, help="file containing the agent goal")
    backend = parser.add_mutually_exclusive_group(required=True)
    backend.add_argument(
        "--model-command",
        help="command that reads a prompt from stdin and writes one SISP action to stdout",
    )
    backend.add_argument(
        "--persistent-model-command",
        help=("long-lived command reading JSON lines with a prompt field and writing "
              "JSON lines with a completion field"),
    )
    parser.add_argument("--workspace", type=Path, default=DEFAULT_WORKSPACE)
    parser.add_argument("--turns", type=int, default=8)
    parser.add_argument("--context", type=int, default=4096)
    parser.add_argument("--model-timeout", type=float, default=120.0)
    parser.add_argument("--sisp-timeout", type=float, default=30.0)
    parser.add_argument("--maximum-response-bytes", type=int, default=65536)
    parser.add_argument("--reset-memory", action="store_true")
    return parser.parse_args()


def main() -> int:
    options = arguments()
    if options.turns <= 0:
        raise SystemExit("--turns must be positive")
    if options.context < 16:
        raise SystemExit("--context must be at least 16")
    command_text = options.model_command or options.persistent_model_command or ""
    command = shlex.split(command_text)
    if not command:
        raise SystemExit("--model-command is empty")
    persistent = None

    workspace = options.workspace.resolve()
    workspace.mkdir(parents=True, exist_ok=True)
    paths = {name: workspace / name for name in (
        "memory.txt", "transcript.txt", "goal.txt", "action.lsp",
        "request.txt", "response.lsp", "status.txt", "final.txt",
        "sync.txt", "sisp.stdout.log", "sisp.stderr.log", "config.lsp",
    )}
    goal_text = (
        options.goal_file.read_text(encoding="utf-8")
        if options.goal_file is not None
        else options.goal
    )
    paths["goal.txt"].write_text(goal_text or "", encoding="utf-8")
    if options.reset_memory or not paths["memory.txt"].exists():
        paths["memory.txt"].write_text("", encoding="utf-8")
    paths["transcript.txt"].touch()
    paths["action.lsp"].write_text("nil", encoding="utf-8")
    paths["config.lsp"].write_text(
        configuration(workspace, options.turns, options.context), encoding="utf-8"
    )

    stdout = paths["sisp.stdout.log"].open("w", encoding="utf-8")
    stderr = paths["sisp.stderr.log"].open("w", encoding="utf-8")
    process = subprocess.Popen(
        [
            str(REPO / "src/sisp"),
            str(paths["config.lsp"]),
            "ml/agent/agent.lsp",
            "ml/agent/external-agent.lsp",
            str(paths["config.lsp"]),
        ],
        cwd=REPO,
        stdin=subprocess.PIPE,
        stdout=stdout,
        stderr=stderr,
        text=True,
    )

    try:
        if options.persistent_model_command is not None:
            persistent = PersistentModel(
                command, options.model_timeout, options.maximum_response_bytes
            )
        send(process, "(external-agent-start)")
        synchronize(process, paths["sync.txt"], "started", options.sisp_timeout)

        while True:
            send(process, "(external-agent-request)")
            synchronize(process, paths["sync.txt"], "requested", options.sisp_timeout)
            status = paths["status.txt"].read_text(encoding="utf-8")
            if status == "done":
                break
            if status != "request":
                raise SystemExit(f"unexpected SISP agent status: {status!r}")

            prompt = paths["request.txt"].read_text(encoding="utf-8")
            response = (
                persistent.complete(prompt) if persistent is not None
                else model_completion(
                    command, prompt, options.model_timeout,
                    options.maximum_response_bytes,
                )
            )
            paths["response.lsp"].write_text(response, encoding="utf-8")
            send(process, "(external-agent-accept)")
            synchronize(process, paths["sync.txt"], "accepted", options.sisp_timeout)
            status = paths["status.txt"].read_text(encoding="utf-8")
            if status == "done":
                break
            if status != "continue":
                raise SystemExit(f"unexpected SISP agent status: {status!r}")

        answer = paths["final.txt"].read_text(encoding="utf-8")
        print(answer)
        print(f"transcript: {paths['transcript.txt']}", file=sys.stderr)
        return 3 if answer == "step-budget-exhausted" else 0
    finally:
        if process.stdin is not None:
            try:
                process.stdin.close()
            except BrokenPipeError:
                pass
        try:
            process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            process.terminate()
            process.wait(timeout=5)
        stdout.close()
        stderr.close()
        if persistent is not None:
            persistent.close()


if __name__ == "__main__":
    raise SystemExit(main())

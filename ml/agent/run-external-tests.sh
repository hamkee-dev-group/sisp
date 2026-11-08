#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

work=$(mktemp -d "${TMPDIR:-/tmp}/sisp-external-agent-tests.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
touch "$work/memory.txt" "$work/transcript.txt" "$work/goal.txt"

config="$work/config.lsp"
printf '%s\n' \
  '(define *matrix-context* 4096)' \
  '(define *matrix-vocabulary* (str-to-list (read-file "ml/llm/vocab-code.txt")))' \
  "(define *agent-memory-path* \"$work/memory.txt\")" \
  "(define *agent-transcript-path* \"$work/transcript.txt\")" \
  "(define *agent-goal-path* \"$work/goal.txt\")" \
  "(define *agent-action-path* \"$work/action.lsp\")" \
  '(define *agent-max-turns* 4)' \
  '(define *agent-action-chars* 0)' \
  '(define *agent-temperature* 0.0)' \
  '(define *agent-top-k* 1)' \
  '(define *agent-top-p* 1.0)' \
  "(define *external-agent-request-path* \"$work/request.txt\")" \
  "(define *external-agent-response-path* \"$work/response.lsp\")" \
  "(define *external-agent-status-path* \"$work/status.txt\")" \
  "(define *external-agent-final-path* \"$work/final.txt\")" >"$config"

output=$(src/sisp \
  ml/llm/metrics.lsp \
  "$config" \
  ml/agent/agent.lsp \
  ml/agent/external-agent.lsp \
  "$config" \
  ml/agent/external-agent-tests.lsp </dev/null)

printf '%s\n' "$output"
if printf '%s\n' "$output" | grep '(fail ' >/dev/null || \
   ! printf '%s\n' "$output" | grep 'failed 0' >/dev/null; then
  printf 'external agent checks failed\n' >&2
  exit 1
fi

python3 ml/agent/test-persistent-backend.py

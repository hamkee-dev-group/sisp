#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

work=$(mktemp -d /tmp/sisp-agent-tests.XXXXXX)
action=ml/agent/generated/current/action.lsp
timeout=${SISP_LLM_TEST_TIMEOUT_SECONDS:-120}
trap 'rm -rf "$work"; rm -f "$action"' EXIT HUP INT TERM
config="$work/config.lsp"
memory="$work/memory.txt"
transcript="$work/transcript.txt"
goal="$work/goal.txt"
touch "$memory" "$transcript" "$goal"
mkdir -p "$(dirname "$action")"

printf '%s\n' \
  '(define *matrix-context* 256)' \
  '(define *matrix-vocabulary* (str-to-list (read-file "ml/llm/vocab-code.txt")))' \
  "(define *agent-memory-path* \"$memory\")" \
  "(define *agent-transcript-path* \"$transcript\")" \
  "(define *agent-goal-path* \"$goal\")" \
  "(define *agent-action-path* \"$action\")" \
  '(define *agent-max-turns* 2)' \
  '(define *agent-action-chars* 32)' \
  '(define *agent-temperature* 0.3)' \
  '(define *agent-top-k* 5)' \
  '(define *agent-top-p* 0.95)' >"$config"

set +e
output=$(ml/llm/run-with-timeout.sh "$timeout" src/sisp \
  ml/llm/math.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-checkpoint.lsp \
  "$config" \
  ml/agent/agent.lsp \
  ml/agent/agent-tests.lsp \
  "$action" \
  ml/agent/agent-tests-final.lsp </dev/null 2>"$work/errors")
status=$?
set -e
printf '%s\n' "$output"
if [ "$status" -ne 0 ]; then
  printf 'SISP exited with status %s\n' "$status" >&2
  cat "$work/errors" >&2
  exit "$status"
fi
if [ -s "$work/errors" ]; then
  cat "$work/errors" >&2
  exit 1
fi
if printf '%s\n' "$output" | grep '(fail ' >/dev/null || \
   ! printf '%s\n' "$output" | grep 'failed 0' >/dev/null; then
  printf 'SISP agent checks failed\n' >&2
  exit 1
fi

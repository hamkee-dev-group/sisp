#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

output=$(mktemp /tmp/sisp-matrix-tests-output.XXXXXX)
errors=$(mktemp /tmp/sisp-matrix-tests-errors.XXXXXX)
timeout=${SISP_LLM_TEST_TIMEOUT_SECONDS:-300}
trap 'rm -f "$output" "$errors"' EXIT HUP INT TERM

set +e
ml/llm/run-with-timeout.sh "$timeout" src/sisp \
  ml/llm/math.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-tests.lsp \
  </dev/null >"$output" 2>"$errors"
status=$?
set -e
if [ "$status" -ne 0 ]; then
  cat "$errors" >&2
  exit "$status"
fi
cat "$output"
if [ -s "$errors" ]; then
  cat "$errors" >&2
  exit 1
fi
if grep '(fail ' "$output" >/dev/null || ! grep 'failed 0' "$output" >/dev/null; then
  printf 'native matrix LLM checks failed\n' >&2
  exit 1
fi

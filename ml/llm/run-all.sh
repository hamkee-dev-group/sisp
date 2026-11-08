#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

timeout_runner=ml/llm/run-with-timeout.sh
test_timeout=${SISP_LLM_TEST_TIMEOUT_SECONDS:-900}

run_checked() {
  name=$1
  shift
  err_file="/tmp/sisp-llm-${name}-err.$$"
  set +e
  output=$("$timeout_runner" "$test_timeout" "$@" 2>"$err_file")
  status=$?
  set -e
  printf '%s\n' "$output"
  if [ "$status" -ne 0 ]; then
    printf 'FAIL: %s exited with status %s\n' "$name" "$status" >&2
    cat "$err_file" >&2
    rm -f "$err_file"
    return "$status"
  fi
  if [ -s "$err_file" ]; then
    printf 'FAIL: %s wrote to stderr\n' "$name" >&2
    cat "$err_file" >&2
    rm -f "$err_file"
    return 1
  fi
  rm -f "$err_file"
  if printf '%s\n' "$output" | grep '(fail ' >/dev/null; then
    printf 'FAIL: %s reported a failed check\n' "$name" >&2
    return 1
  fi
  if ! printf '%s\n' "$output" | grep 'failed 0' >/dev/null; then
    printf 'FAIL: %s did not report a test summary\n' "$name" >&2
    return 1
  fi
}

run_checked core ml/llm/run-tests.sh
run_checked attention src/sisp ml/llm/attention.lsp </dev/null
run_checked xor ml/llm/run-xor.sh
run_checked bigram ml/llm/run-charlm.sh
run_checked reinforce ml/llm/run-reinforce.sh
run_checked transformer ml/llm/run-transformer.sh
run_checked corpus env SISP_LLM_STEPS=5 SISP_LLM_TOKENS=1 \
  ml/llm/run-corpus-transformer.sh
run_checked matrix-calculus ml/llm/run-matrix-tests.sh
run_checked matrix-learning ml/llm/run-matrix-learning-test.sh
python3 ml/llm/test_prepare_corpus.py
run_checked matrix-checkpoint ml/llm/run-matrix-checkpoint-tests.sh
run_checked matrix-corpus env SISP_LLM_STEPS=3 SISP_LLM_TOKENS=1 \
  ml/llm/run-matrix-corpus.sh
run_checked observer ml/llm/run-observer-tests.sh
run_checked agent ml/agent/run-tests.sh
ml/agent/run-protocol-tests.sh

printf '%s\n' 'ALL LLM CHECKS PASSED'

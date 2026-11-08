#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

errors=$(mktemp /tmp/sisp-llm-observer-errors.XXXXXX)
workspace=$(mktemp -d /tmp/sisp-llm-observer-checkpoint.XXXXXX)
checkpoint="$workspace/model"
trap 'rm -f "$errors"; rm -rf "$workspace"' EXIT HUP INT TERM

env \
  SISP_LLM_PROFILE=small \
  SISP_LLM_STEPS=1 \
  SISP_LLM_TOKENS=1 \
  SISP_LLM_EVAL_SEQUENCES=1 \
  SISP_LLM_SAVE="$checkpoint" \
  ml/llm/run-matrix-corpus.sh >/dev/null 2>"$errors"

if [ -s "$errors" ]; then
  cat "$errors" >&2
  exit 1
fi
: >"$errors"

output=$(env \
  SISP_LLM_PROFILE=small \
  SISP_LLM_STEPS=0 \
  SISP_LLM_TOKENS=1 \
  SISP_LLM_EVAL_SEQUENCES=1 \
  SISP_LLM_RESUME="$checkpoint" \
  SISP_LLM_OBSERVER=1 \
  ml/llm/run-matrix-corpus.sh 2>"$errors")

printf '%s\n' "$output"
if [ -s "$errors" ]; then
  cat "$errors" >&2
  exit 1
fi

for required in \
  'observer-training step 1' \
  'observer-contract parameter-names-match t parameters-finite t context-matches-model t vocabulary-matches-model t' \
  'observer-integrity starting-weight-norm' \
  'weights-unchanged t' \
  'observer-finished read-only t'
do
  if ! printf '%s\n' "$output" | grep "$required" >/dev/null; then
    printf 'observer smoke test missing: %s\n' "$required" >&2
    exit 1
  fi
done

printf '%s\n' '(test-summary passed 5 failed 0)'

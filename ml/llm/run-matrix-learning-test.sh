#!/bin/sh
# A deterministic end-to-end regression that proves the native model learns,
# separate from production validation/model-selection logic.
set -eu

cd "$(dirname "$0")/../.."

output=$(SISP_LLM_PROFILE=small SISP_LLM_STEPS=40 SISP_LLM_TOKENS=1 \
  SISP_LLM_EVAL_SEQUENCES=8 ml/llm/run-matrix-corpus.sh)
initial=$(printf '%s\n' "$output" | awk '{for (i=1; i<NF; i++) if ($i == "(initial-training-nll") {print $(i+1); exit}}')
final=$(printf '%s\n' "$output" | awk '{for (i=1; i<NF; i++) if ($i == "(final-training-nll") {print $(i+1); exit}}')
if ! awk -v initial="$initial" -v final="$final" \
    'BEGIN {exit !(initial ~ /^[0-9.]+$/ && final ~ /^[0-9.]+$/ && final < initial)}'; then
  printf 'FAIL: deterministic learning regression did not lower training NLL (%s -> %s)\n' \
    "$initial" "$final" >&2
  exit 1
fi
printf '(pass "native transformer lowers deterministic held sample NLL: %s -> %s")\n' \
  "$initial" "$final"
printf '%s\n' '(tests passed 1 failed 0)'

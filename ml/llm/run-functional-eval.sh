#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

# Require a checkpoint unless explicitly testing the random smoke model.
if [ -z "${SISP_LLM_RESUME:-}" ] && [ "${SISP_LLM_ALLOW_RANDOM:-0}" != 1 ]; then
  printf 'SISP_LLM_RESUME is required for native functional evaluation\n' >&2
  exit 2
fi

work=$(mktemp -d /tmp/sisp-functional-eval.XXXXXX)
trap 'rm -rf "$work"' EXIT HUP INT TERM
mkdir -p "$work/candidates"

SISP_LLM_STEPS=0 \
SISP_LLM_TOKENS=${SISP_LLM_TOKENS:-1} \
SISP_LLM_EVAL_SEQUENCES=${SISP_LLM_EVAL_SEQUENCES:-8} \
SISP_LLM_FUNCTIONAL_EVAL=1 \
SISP_LLM_FUNCTIONAL_OUTPUT="$work/candidates" \
ml/llm/run-matrix-corpus.sh

passed=0
failed=0
for check in ml/llm/eval/*.check.lsp; do
  name=$(basename "$check" .check.lsp)
  candidate="$work/candidates/$name.lsp"
  errors="$work/$name.err"
  if [ ! -f "$candidate" ]; then
    failed=$((failed + 1))
    printf '(functional-execution-fail "%s" "candidate missing")\n' "$name"
    continue
  fi
  result=$({ cat "$candidate"; cat "$check"; } | src/sisp 2>"$errors" \
    | grep -vE '^:?[[:space:]]*$' | tail -1 | sed -E 's/^:[[:space:]]*//; s/[[:space:]]+$//' || true)
  if [ ! -s "$errors" ] && [ "$result" = t ]; then
    passed=$((passed + 1))
    printf '(functional-execution-pass "%s")\n' "$name"
  else
    failed=$((failed + 1))
    printf '(functional-execution-fail "%s" "%s")\n' "$name" "$result"
  fi
done
printf '(functional-execution-summary passed %s failed %s)\n' "$passed" "$failed"
[ "$failed" -eq 0 ]

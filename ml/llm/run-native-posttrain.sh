#!/bin/sh
# Mechanical batch boundary for the SISP-defined native posttraining program.
set -eu

cd "$(dirname "$0")/../.."

config=ml/llm/native-posttrain-config.lsp
plan=$(mktemp /tmp/sisp-native-posttrain-plan.XXXXXX)
plan_path=$(mktemp /tmp/sisp-native-posttrain-plan-path.XXXXXX)
errors=$(mktemp /tmp/sisp-native-posttrain-errors.XXXXXX)
trap 'rm -f "$plan" "$plan_path" "$errors"' EXIT HUP INT TERM

printf '(define *native-posttrain-plan-path* "%s")\n' "$plan" >"$plan_path"
src/sisp \
  ml/llm/math.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-checkpoint.lsp \
  "$config" "$plan_path" ml/llm/native-posttrain-plan.lsp \
  </dev/null >/dev/null

checkpoint=$(awk -F '|' '{print $1}' "$plan")
output=$(awk -F '|' '{print $2}' "$plan")
mode=$(awk -F '|' '{print $3}' "$plan")
generation=$(awk -F '|' '{print $4}' "$plan")
prompt=$(awk -F '|' '{print $5}' "$plan")
chosen=$(awk -F '|' '{print $6}' "$plan")
rejected=$(awk -F '|' '{print $7}' "$plan")
check=$(awk -F '|' '{print $8}' "$plan")
reward_path=$(awk -F '|' '{print $9}' "$plan")
pass_reward=$(awk -F '|' '{print $10}' "$plan")
failure_reward=$(awk -F '|' '{print $11}' "$plan")

[ -f "$checkpoint/state.lsp" ] || {
  printf 'configured checkpoint is missing: %s\n' "$checkpoint" >&2; exit 2; }
[ -f "$prompt" ] || {
  printf 'configured prompt is missing: %s\n' "$prompt" >&2; exit 2; }
if [ "$mode" = sft ]; then
  [ -f "$chosen" ] || { printf 'configured completion is missing: %s\n' "$chosen" >&2; exit 2; }
fi
if [ "$mode" = dpo ]; then
  [ -f "$chosen" ] && [ -f "$rejected" ] || {
    printf 'configured DPO chosen/rejected file is missing\n' >&2; exit 2; }
fi
if [ -e "$output" ]; then
  printf 'configured output already exists; edit native-posttrain-config.lsp: %s\n' "$output" >&2
  exit 2
fi
mkdir -p "$(dirname "$output")" "$output" "$(dirname "$chosen")" "$(dirname "$reward_path")"

set -- src/sisp \
  ml/llm/math.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-checkpoint.lsp \
  ml/llm/interactive-knowledge-config.lsp \
  ml/llm/data/local/sisp-knowledge-v1/train-spans.lsp \
  ml/llm/data/local/sisp-knowledge-v1/valid-spans.lsp \
  ml/llm/matrix-corpus.lsp
for part in "$checkpoint"/part-*.lsp; do set -- "$@" "$part"; done

if [ "$mode" = reinforce ] && [ "$generation" = generate ]; then
  [ -f "$check" ] || { printf 'configured check is missing: %s\n' "$check" >&2; exit 2; }
  if ! "$@" "$checkpoint/state.lsp" "$config" \
    ml/llm/functional-eval.lsp \
    ml/llm/native-posttrain-generate.lsp \
    </dev/null 2>"$errors"; then
    cat "$errors" >&2
    exit 1
  fi
  result=$({ cat "$prompt"; cat "$chosen"; cat "$check"; } | src/sisp 2>"$errors" \
    | grep -vE '^:?[[:space:]]*$' | tail -1 \
    | sed -E 's/^:[[:space:]]*//; s/[[:space:]]+$//' || true)
  if [ ! -s "$errors" ] && [ "$result" = t ]; then
    printf '%s' "$pass_reward" >"$reward_path"
  else
    printf '%s' "$failure_reward" >"$reward_path"
  fi
  : >"$errors"
fi

set -- "$@" "$checkpoint/state.lsp" "$config" \
  ml/llm/posttrain.lsp \
  ml/llm/posttrain-session.lsp \
  ml/llm/native-posttrain-main.lsp \
  ml/llm/native-posttrain-save.lsp

if ! "$@" </dev/null 2>"$errors"; then
  cat "$errors" >&2
  exit 1
fi
if [ -s "$errors" ]; then
  cat "$errors" >&2
  exit 1
fi
printf 'native SISP posttraining checkpoint: %s\n' "$output"

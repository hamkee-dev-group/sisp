#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

profile=${SISP_LLM_PROFILE:-small}
case "$profile" in
  small)
    context=8 dimension=8 hidden=16 learning_rate=0.015 default_steps=600 ;;
  practical)
    context=16 dimension=16 hidden=32 learning_rate=0.008 default_steps=1200 ;;
  research)
    context=24 dimension=24 hidden=48 learning_rate=0.005 default_steps=1200 ;;
  *)
    printf 'unknown SISP_LLM_PROFILE: %s\n' "$profile" >&2
    exit 2 ;;
esac
steps=${SISP_LLM_STEPS:-$default_steps}
tokens=${SISP_LLM_TOKENS:-72}

case "$steps" in
  ''|*[!0-9]*)
    printf 'SISP_LLM_STEPS must be a positive integer\n' >&2
    exit 2 ;;
esac
case "$tokens" in
  ''|*[!0-9]*)
    printf 'SISP_LLM_TOKENS must be a positive integer\n' >&2
    exit 2 ;;
esac
if [ "$steps" -eq 0 ] || [ "$tokens" -eq 0 ]; then
  printf 'SISP_LLM_STEPS and SISP_LLM_TOKENS must be greater than zero\n' >&2
  exit 2
fi

config=$(mktemp /tmp/sisp-llm-config.XXXXXX)
schedule=$(mktemp /tmp/sisp-llm-schedule.XXXXXX)
generation=$(mktemp /tmp/sisp-llm-generation.XXXXXX)
errors=$(mktemp /tmp/sisp-llm-errors.XXXXXX)
trap 'rm -f "$config" "$schedule" "$generation" "$errors"' EXIT HUP INT TERM

printf '%s\n' \
  "(define *corpus-profile* (quote $profile))" \
  "(define *corpus-context* $context)" \
  "(define *corpus-dimension* $dimension)" \
  "(define *corpus-hidden* $hidden)" \
  "(define *corpus-learning-rate* $learning_rate)" \
  "(define *corpus-generation-count* $tokens)" >"$config"

awk -v steps="$steps" 'BEGIN {
  for (i = 0; i < steps; i++) print "(corpus-training-step)"
}' >"$schedule"
awk -v tokens="$tokens" 'BEGIN {
  for (i = 0; i < tokens; i++) print "(corpus-generation-step)"
}' >"$generation"

src/sisp \
  ml/llm/engine.lsp \
  ml/llm/math.lsp \
  ml/llm/tensor.lsp \
  ml/llm/optim.lsp \
  ml/llm/data.lsp \
  ml/llm/nn.lsp \
  ml/llm/model.lsp \
  ml/llm/metrics.lsp \
  "$config" \
  ml/llm/corpus-transformer.lsp \
  "$schedule" \
  ml/llm/corpus-transformer-generate.lsp \
  "$generation" \
  ml/llm/corpus-transformer-report.lsp \
  </dev/null 2>"$errors"
if [ -s "$errors" ]; then
  cat "$errors" >&2
  exit 1
fi

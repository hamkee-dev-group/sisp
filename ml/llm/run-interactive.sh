#!/bin/sh
# Human terminal for native, pure-SISP code and knowledge models.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
cd "$ROOT"

model=${SISP_LLM_INTERACTIVE_MODEL:-code}
case "$model" in
  code)
    default_checkpoint=ml/llm/checkpoints/code-best
    model_config=ml/llm/interactive-config.lsp
    ;;
  knowledge)
    default_checkpoint=ml/llm/checkpoints/knowledge-best
    model_config=ml/llm/interactive-knowledge-config.lsp
    ;;
  *)
    printf 'SISP_LLM_INTERACTIVE_MODEL must be code or knowledge\n' >&2
    exit 2
    ;;
esac
checkpoint=${SISP_LLM_INTERACTIVE_CHECKPOINT:-$default_checkpoint}
count=${SISP_LLM_INTERACTIVE_COUNT:-96}
sampling=0
newline='
'

if [ ! -d "$checkpoint" ] || [ ! -f "$checkpoint/state.lsp" ]; then
  printf 'native checkpoint is missing: %s\n' "$checkpoint" >&2
  exit 2
fi

prompt_file=$(mktemp /tmp/sisp-llm-prompt.XXXXXX)
output_file=$(mktemp /tmp/sisp-llm-output.XXXXXX)
query_config=$(mktemp /tmp/sisp-llm-query.XXXXXX)
schedule=$(mktemp /tmp/sisp-llm-schedule.XXXXXX)
errors=$(mktemp /tmp/sisp-llm-errors.XXXXXX)
trap 'rm -f "$prompt_file" "$output_file" "$query_config" "$schedule" "$errors"' EXIT HUP INT TERM

run_prompt()
{
  prompt=$1
  printf '%s' "$prompt" >"$prompt_file"
  : >"$output_file"
  : >"$schedule"

  if [ "$sampling" -eq 1 ]; then
    temperature=0.3
    top_k=5
    top_p=0.95
  else
    temperature=0.3
    top_k=1
    top_p=1.0
  fi

  printf '%s\n' \
    "(define *llm-interactive-prompt-path* \"$prompt_file\")" \
    "(define *llm-interactive-output-path* \"$output_file\")" \
    "(define *llm-interactive-temperature* $temperature)" \
    "(define *llm-interactive-top-k* $top_k)" \
    "(define *llm-interactive-top-p* $top_p)" >"$query_config"

  i=0
  while [ "$i" -lt "$count" ]; do
    printf '%s\n' '(llm-interactive-step)' >>"$schedule"
    i=$((i + 1))
  done
  printf '%s\n' '(llm-interactive-finish)' >>"$schedule"

  set -- src/sisp \
    ml/llm/math.lsp \
    ml/llm/data.lsp \
    ml/llm/metrics.lsp \
    ml/llm/matrix-engine.lsp \
    ml/llm/matrix-nn.lsp \
    ml/llm/matrix-model.lsp \
    ml/llm/matrix-checkpoint.lsp \
    "$model_config" \
    ml/llm/matrix-inference.lsp

  for part in "$checkpoint"/part-*.lsp; do
    set -- "$@" "$part"
  done

  set -- "$@" \
    "$checkpoint/state.lsp" \
    "$query_config" \
    ml/llm/interactive.lsp \
    "$schedule" \
    ml/llm/interactive-report.lsp

  if ! "$@" </dev/null >/dev/null 2>"$errors"; then
    cat "$errors" >&2
    return 1
  fi
  if [ -s "$errors" ]; then
    cat "$errors" >&2
    return 1
  fi
  cat "$output_file"
  printf '\n'
}

# A command-line argument is a convenient one-shot completion.
if [ "$#" -gt 0 ]; then
  run_prompt "$*"
  exit $?
fi

printf 'Native SISP %s model. Enter a prefix; :help lists commands.\n' "$model"
while :; do
  printf 'sisp-llm> '
  if ! IFS= read -r line; then
    printf '\n'
    break
  fi
  case "$line" in
    :quit|:q)
      break
      ;;
    :help)
      printf '%s\n' \
        ':help NAME       explain a SISP name' \
        ':algorithm NAME  complete a named algorithm' \
        ':ask QUESTION    ask the knowledge model a question' \
        ':count N         generate N characters' \
        ':sample on|off   stochastic or deterministic generation' \
        ':quit            exit' \
        'Anything else is used directly as a SISP code prefix.'
      ;;
    ':help '*)
      run_prompt "; help: ${line#':help '}"
      ;;
    ':algorithm '*)
      run_prompt "; algorithm: ${line#':algorithm '}"
      ;;
    ':ask '*)
      run_prompt "; prompt: ${line#':ask '}${newline}; response:"
      ;;
    ':count '*)
      requested=${line#':count '}
      case "$requested" in
        ''|*[!0-9]*) printf '%s\n' 'count must be a positive integer' ;;
        0) printf '%s\n' 'count must be a positive integer' ;;
        *) count=$requested; printf 'generation count: %s\n' "$count" ;;
      esac
      ;;
    ':sample on')
      sampling=1
      printf '%s\n' 'sampling: on'
      ;;
    ':sample off')
      sampling=0
      printf '%s\n' 'sampling: off'
      ;;
    *)
      run_prompt "$line"
      ;;
  esac
done

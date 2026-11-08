#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

checkpoint_integrity=ml/llm/checkpoint-integrity.py

profile=${SISP_LLM_PROFILE:-code}
case "$profile" in
  small)
    context=16 dimension=16 heads=2 layers=1 hidden=64
    init_scale=0.08 learning_rate=0.002 weight_decay=0.001 evaluation_count=8
    parameter_objects=20 expected_parameters=6176 microbatch=2 warmup=20 decay=5000 ;;
  practical)
    context=32 dimension=64 heads=4 layers=4 hidden=256
    init_scale=0.04 learning_rate=0.001 weight_decay=0.01 evaluation_count=16
    parameter_objects=100 expected_parameters=273312 microbatch=4 warmup=100 decay=50000 ;;
  code)
    context=256 dimension=96 heads=4 layers=4 hidden=256
    init_scale=0.03 learning_rate=0.001 weight_decay=0.05 evaluation_count=32
    parameter_objects=100 expected_parameters=479552 microbatch=4 warmup=500 decay=40000 ;;
  code-long)
    context=512 dimension=96 heads=4 layers=4 hidden=256
    init_scale=0.03 learning_rate=0.001 weight_decay=0.05 evaluation_count=32
    parameter_objects=100 expected_parameters=504128 microbatch=2 warmup=500 decay=40000 ;;
  code-plus)
    context=256 dimension=128 heads=4 layers=4 hidden=384
    init_scale=0.025 learning_rate=0.0007 weight_decay=0.05 evaluation_count=32
    parameter_objects=100 expected_parameters=901856 microbatch=2 warmup=1000 decay=80000 ;;
  research)
    context=128 dimension=128 heads=8 layers=6 hidden=512
    init_scale=0.025 learning_rate=0.0005 weight_decay=0.05 evaluation_count=16
    parameter_objects=244 expected_parameters=1610208 microbatch=4 warmup=1000 decay=100000 ;;
  large)
    context=128 dimension=256 heads=8 layers=8 hidden=1024
    init_scale=0.018 learning_rate=0.0003 weight_decay=0.05 evaluation_count=8
    parameter_objects=324 expected_parameters=8468832 microbatch=2 warmup=500 decay=50000 ;;
  *) printf 'unknown SISP_LLM_PROFILE: %s\n' "$profile" >&2; exit 2 ;;
esac

resume=${SISP_LLM_RESUME:-}
allow_random=${SISP_AGENT_ALLOW_RANDOM:-0}
goal=${SISP_AGENT_GOAL_FILE:-}
turns=${SISP_AGENT_TURNS:-3}
action_chars=${SISP_AGENT_ACTION_CHARS:-96}
memory=${SISP_AGENT_MEMORY:-ml/agent/memory/default.txt}
transcript=${SISP_AGENT_TRANSCRIPT:-ml/agent/generated/current/transcript.txt}
temperature=${SISP_AGENT_TEMPERATURE:-0.3}
top_k=${SISP_AGENT_TOP_K:-5}
top_p=${SISP_AGENT_TOP_P:-0.95}
timeout=${SISP_AGENT_TIMEOUT_SECONDS:-1800}

vocabulary=${SISP_LLM_VOCAB:-ml/llm/vocab-code.txt}
learning_rate=${SISP_LLM_LEARNING_RATE:-$learning_rate}
weight_decay=${SISP_LLM_WEIGHT_DECAY:-$weight_decay}
microbatch=${SISP_LLM_MICROBATCH:-$microbatch}
warmup=${SISP_LLM_WARMUP_STEPS:-$warmup}
decay=${SISP_LLM_DECAY_STEPS:-$decay}
min_lr=${SISP_LLM_MIN_LR_RATIO:-0.1}
clip_norm=${SISP_LLM_CLIP_NORM:-1.0}
beta2=${SISP_LLM_BETA2:-0.95}
sampling=${SISP_LLM_SAMPLING:-shuffle}
sampler_seed=${SISP_LLM_SAMPLER_SEED:-104729}

case "$allow_random" in 0|1) : ;; *) printf 'SISP_AGENT_ALLOW_RANDOM must be 0 or 1\n' >&2; exit 2;; esac
case "$turns" in ''|*[!0-9]*) printf 'SISP_AGENT_TURNS must be a positive integer\n' >&2; exit 2;; esac
case "$action_chars" in ''|*[!0-9]*) printf 'SISP_AGENT_ACTION_CHARS must be a positive integer\n' >&2; exit 2;; esac
case "$top_k" in ''|*[!0-9]*) printf 'SISP_AGENT_TOP_K must be a positive integer\n' >&2; exit 2;; esac
case "$timeout" in ''|*[!0-9]*) printf 'SISP_AGENT_TIMEOUT_SECONDS must be a positive integer\n' >&2; exit 2;; esac
if [ "$turns" -eq 0 ] || [ "$action_chars" -eq 0 ] || [ "$top_k" -eq 0 ] || [ "$timeout" -eq 0 ]; then
  printf 'agent turns, action characters, top-k, and timeout must be greater than zero\n' >&2
  exit 2
fi
if [ "$action_chars" -gt 256 ]; then
  printf 'SISP_AGENT_ACTION_CHARS must not exceed 256\n' >&2
  exit 2
fi
if [ -z "$goal" ] || [ ! -f "$goal" ]; then
  printf 'SISP_AGENT_GOAL_FILE must name an existing text file\n' >&2
  exit 2
fi
if [ -z "$resume" ] && [ "$allow_random" -ne 1 ]; then
  printf 'SISP_LLM_RESUME is required; set SISP_AGENT_ALLOW_RANDOM=1 only for smoke tests\n' >&2
  exit 2
fi
if [ -n "$resume" ] && { [ ! -d "$resume" ] || [ ! -f "$resume/state.lsp" ]; }; then
  printf 'SISP_LLM_RESUME is not a complete checkpoint: %s\n' "$resume" >&2
  exit 2
fi
if [ -n "$resume" ] && ! python3 "$checkpoint_integrity" verify "$resume"; then
  printf 'SISP_LLM_RESUME failed checkpoint integrity verification: %s\n' "$resume" >&2
  exit 2
fi
case "$sampling" in shuffle|sequential) : ;; *) printf 'SISP_LLM_SAMPLING must be shuffle or sequential\n' >&2; exit 2;; esac
if ! awk -v value="$temperature" 'BEGIN {exit !(value ~ /^[0-9]+([.][0-9]+)?$/ && value > 0)}'; then
  printf 'SISP_AGENT_TEMPERATURE must be positive\n' >&2; exit 2
fi
if ! awk -v value="$top_p" 'BEGIN {exit !(value ~ /^(0[.][0-9]+|1([.]0+)?)$/ && value > 0 && value <= 1)}'; then
  printf 'SISP_AGENT_TOP_P must be greater than zero and at most one\n' >&2; exit 2
fi
if [ "$top_k" -gt 96 ]; then
  printf 'SISP_AGENT_TOP_K cannot exceed the frozen vocabulary size\n' >&2; exit 2
fi

for path in "$goal" "$memory" "$transcript" "$vocabulary"; do
  case "$path" in *'"'*|*'\'*|*'
'*) printf 'agent paths cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
done
for path in "$vocabulary"; do
  if [ ! -f "$path" ]; then printf 'required file not found: %s\n' "$path" >&2; exit 2; fi
done

generated=ml/agent/generated/current
lock=ml/agent/generated/agent.lock
mkdir -p "$generated" "$(dirname "$memory")" "$(dirname "$transcript")"
touch "$memory" "$transcript"
if ! mkdir "$lock" 2>/dev/null; then
  printf 'another SISP agent owns %s\n' "$lock" >&2
  exit 2
fi

config=$(mktemp /tmp/sisp-agent-config.XXXXXX)
start_schedule=$(mktemp /tmp/sisp-agent-start.XXXXXX)
turn_schedule=$(mktemp /tmp/sisp-agent-turn.XXXXXX)
post_schedule=$(mktemp /tmp/sisp-agent-post.XXXXXX)
report_schedule=$(mktemp /tmp/sisp-agent-report.XXXXXX)
output=$(mktemp /tmp/sisp-agent-output.XXXXXX)
errors=$(mktemp /tmp/sisp-agent-errors.XXXXXX)
trap 'rm -f "$config" "$start_schedule" "$turn_schedule" "$post_schedule" "$report_schedule" "$output" "$errors"; rm -rf "$lock"' EXIT HUP INT TERM

vocabulary_checksum=$(cksum <"$vocabulary" | awk '{print $1}')

printf '%s\n' \
  "(define *matrix-profile* (quote $profile))" \
  "(define *matrix-profile-name* \"$profile\")" \
  "(define *matrix-context* $context)" \
  "(define *matrix-dimension* $dimension)" \
  "(define *matrix-heads* $heads)" \
  "(define *matrix-layers* $layers)" \
  "(define *matrix-hidden* $hidden)" \
  "(define *matrix-init-scale* $init_scale)" \
  "(define *matrix-learning-rate* $learning_rate)" \
  "(define *matrix-weight-decay* $weight_decay)" \
  "(define *matrix-evaluation-count* $evaluation_count)" \
  "(define *matrix-expected-parameter-objects* $parameter_objects)" \
  "(define *matrix-expected-parameters* $expected_parameters)" \
  "(define *matrix-vocabulary-path* \"$vocabulary\")" \
  "(define *matrix-vocabulary-checksum* $vocabulary_checksum)" \
  '(define *matrix-corpus-mode* (quote strict))' \
  "(define *matrix-sampling-mode* (quote $sampling))" \
  "(define *matrix-sampling-mode-name* \"$sampling\")" \
  "(define *matrix-sampler-seed* $sampler_seed)" \
  "(define *matrix-microbatch* $microbatch)" \
  "(define *matrix-warmup-steps* $warmup)" \
  "(define *matrix-decay-steps* $decay)" \
  "(define *matrix-min-learning-rate-ratio* $min_lr)" \
  "(define *matrix-clip-norm* $clip_norm)" \
  "(define *matrix-beta2* $beta2)" \
  '(define *matrix-log-every* 50)' \
  "(define *matrix-temperature* $temperature)" \
  "(define *matrix-top-k* $top_k)" \
  "(define *matrix-top-p* $top_p)" \
  "(define *agent-goal-path* \"$goal\")" \
  "(define *agent-memory-path* \"$memory\")" \
  "(define *agent-transcript-path* \"$transcript\")" \
  "(define *agent-action-path* \"$generated/action.lsp\")" \
  "(define *agent-max-turns* $turns)" \
  "(define *agent-action-chars* $action_chars)" \
  "(define *agent-temperature* $temperature)" \
  "(define *agent-top-k* $top_k)" \
  "(define *agent-top-p* $top_p)" >"$config"

printf '%s\n' '(agent-start)' >"$start_schedule"
awk -v chars="$action_chars" 'BEGIN {
  print "(agent-prepare-action)"
  for (i = 0; i < chars; i++) print "(agent-generation-step)"
  print "(agent-finish-action)"
}' >"$turn_schedule"
printf '%s\n' \
  '(agent-validate-action)' \
  '(agent-record-turn)' \
  '(agent-advance-turn)' >"$post_schedule"
printf '%s\n' '(agent-report)' >"$report_schedule"

# SISP opens command-line inputs in order. Each turn writes this file before
# the following command-line slot reads it, avoiding a nested evaluator/load.
touch "$generated/action.lsp"

set -- src/sisp \
  ml/llm/math.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-checkpoint.lsp \
  "$config" \
  ml/llm/matrix-inference.lsp

if [ -n "$resume" ]; then
  for part in "$resume"/part-*.lsp; do set -- "$@" "$part"; done
  set -- "$@" "$resume/state.lsp"
fi

set -- "$@" ml/agent/agent.lsp "$start_schedule"
turn=0
while [ "$turn" -lt "$turns" ]; do
  set -- "$@" "$turn_schedule" "$generated/action.lsp" "$post_schedule"
  turn=$((turn + 1))
done
set -- "$@" "$report_schedule"
set +e
ml/llm/run-with-timeout.sh "$timeout" "$@" \
  </dev/null >"$output" 2>"$errors"
status=$?
set -e
cat "$output"
if [ "$status" -eq 124 ]; then
  printf 'agent exceeded SISP_AGENT_TIMEOUT_SECONDS=%s\n' "$timeout" >&2
  cat "$errors" >&2
  exit 124
fi
if [ "$status" -ne 0 ]; then
  printf 'agent interpreter exited with status %s\n' "$status" >&2
  cat "$errors" >&2
  exit "$status"
fi
if [ -s "$errors" ]; then
  printf '%s\n' '=== generated-action/interpreter diagnostics ===' >&2
  cat "$errors" >&2
fi
if ! grep '(agent-finished ' "$output" >/dev/null; then
  printf 'agent did not reach its final report\n' >&2
  exit 1
fi
if [ -s "$errors" ]; then
  printf 'agent completed with generated-action diagnostics\n' >&2
  exit 1
fi

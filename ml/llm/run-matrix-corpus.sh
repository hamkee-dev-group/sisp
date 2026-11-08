#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

checkpoint_integrity=ml/llm/checkpoint-integrity.py

profile=${SISP_LLM_PROFILE:-small}
case "$profile" in
  small)
    context=16 dimension=16 heads=2 layers=1 hidden=64
    init_scale=0.08 learning_rate=0.002 weight_decay=0.001 evaluation_count=8
    default_steps=500 parameter_objects=20 expected_parameters=6176 default_microbatch=2
    default_warmup=20 default_decay=5000 ;;
  practical)
    context=32 dimension=64 heads=4 layers=4 hidden=256
    init_scale=0.04 learning_rate=0.001 weight_decay=0.01 evaluation_count=16
    default_steps=1000 parameter_objects=100 expected_parameters=273312 default_microbatch=4
    default_warmup=100 default_decay=50000 ;;
  code)
    context=256 dimension=96 heads=4 layers=4 hidden=256
    init_scale=0.03 learning_rate=0.001 weight_decay=0.05 evaluation_count=32
    default_steps=1000 parameter_objects=100 expected_parameters=479552 default_microbatch=4
    default_warmup=500 default_decay=40000 ;;
  code-long)
    context=512 dimension=96 heads=4 layers=4 hidden=256
    init_scale=0.03 learning_rate=0.001 weight_decay=0.05 evaluation_count=32
    default_steps=500 parameter_objects=100 expected_parameters=504128 default_microbatch=2
    default_warmup=500 default_decay=40000 ;;
  code-plus)
    context=256 dimension=128 heads=4 layers=4 hidden=384
    init_scale=0.025 learning_rate=0.0007 weight_decay=0.05 evaluation_count=32
    default_steps=500 parameter_objects=100 expected_parameters=901856 default_microbatch=2
    # This profile processes half as many target characters per update as
    # `code`; double the step schedule so equal-character A/B runs see the
    # same warmup/decay progress.
    default_warmup=1000 default_decay=80000 ;;
  research)
    context=128 dimension=128 heads=8 layers=6 hidden=512
    init_scale=0.025 learning_rate=0.0005 weight_decay=0.05 evaluation_count=16
    default_steps=1000 parameter_objects=244 expected_parameters=1610208 default_microbatch=4
    default_warmup=1000 default_decay=100000 ;;
  large)
    context=128 dimension=256 heads=8 layers=8 hidden=1024
    init_scale=0.018 learning_rate=0.0003 weight_decay=0.05 evaluation_count=8
    default_steps=250 parameter_objects=324 expected_parameters=8468832 default_microbatch=2
    default_warmup=500 default_decay=50000 ;;
  *)
    printf 'unknown SISP_LLM_PROFILE: %s\n' "$profile" >&2
    exit 2 ;;
esac

steps=${SISP_LLM_STEPS:-$default_steps}
tokens=${SISP_LLM_TOKENS:-72}
resume=${SISP_LLM_RESUME:-}
save=${SISP_LLM_SAVE:-}
prompt_file=${SISP_LLM_PROMPT_FILE:-}
corpus=${SISP_LLM_CORPUS:-ml/llm/corpus.txt}
validation_corpus=${SISP_LLM_VALID_CORPUS:-ml/llm/corpus-valid.txt}
test_corpus=${SISP_LLM_TEST_CORPUS:-}
vocabulary=${SISP_LLM_VOCAB:-ml/llm/vocab-code.txt}
corpus_spans=${SISP_LLM_CORPUS_SPANS:-}
validation_spans=${SISP_LLM_VALID_SPANS:-}
test_spans=${SISP_LLM_TEST_SPANS:-}
if [ -z "$corpus_spans" ]; then
  candidate=${corpus%.*}-spans.lsp
  [ -f "$candidate" ] && corpus_spans=$candidate
fi
if [ -z "$validation_spans" ]; then
  candidate=${validation_corpus%.*}-spans.lsp
  [ -f "$candidate" ] && validation_spans=$candidate
fi
if [ -n "$test_corpus" ] && [ -z "$test_spans" ]; then
  candidate=${test_corpus%.*}-spans.lsp
  [ -f "$candidate" ] && test_spans=$candidate
fi
corpus_mode=${SISP_LLM_CORPUS_MODE:-strict}
microbatch=${SISP_LLM_MICROBATCH:-$default_microbatch}
warmup_steps=${SISP_LLM_WARMUP_STEPS:-$default_warmup}
decay_steps=${SISP_LLM_DECAY_STEPS:-$default_decay}
min_learning_rate_ratio=${SISP_LLM_MIN_LR_RATIO:-0.1}
clip_norm=${SISP_LLM_CLIP_NORM:-1.0}
learning_rate=${SISP_LLM_LEARNING_RATE:-$learning_rate}
weight_decay=${SISP_LLM_WEIGHT_DECAY:-$weight_decay}
log_every=${SISP_LLM_LOG_EVERY:-50}
temperature=${SISP_LLM_TEMPERATURE:-0.3}
top_k=${SISP_LLM_TOP_K:-5}
top_p=${SISP_LLM_TOP_P:-0.95}
beta2=${SISP_LLM_BETA2:-0.95}
sampling=${SISP_LLM_SAMPLING:-shuffle}
sampler_seed=${SISP_LLM_SAMPLER_SEED:-104729}
evaluation_count=${SISP_LLM_EVAL_SEQUENCES:-$evaluation_count}
observer=${SISP_LLM_OBSERVER:-0}
observer_full=${SISP_LLM_OBSERVER_FULL:-0}
functional_eval=${SISP_LLM_FUNCTIONAL_EVAL:-0}
functional_output=${SISP_LLM_FUNCTIONAL_OUTPUT:-}
functional_tasks=${SISP_LLM_FUNCTIONAL_TASKS:-ml/llm/functional-eval-tasks.lsp}
case "$steps" in ''|*[!0-9]*) printf 'SISP_LLM_STEPS must be a non-negative integer\n' >&2; exit 2;; esac
case "$tokens" in ''|*[!0-9]*) printf 'SISP_LLM_TOKENS must be a positive integer\n' >&2; exit 2;; esac
if [ "$tokens" -eq 0 ]; then
  printf 'SISP_LLM_TOKENS must be greater than zero\n' >&2
  exit 2
fi
case "$microbatch" in ''|*[!0-9]*) printf 'SISP_LLM_MICROBATCH must be a positive integer\n' >&2; exit 2;; esac
case "$warmup_steps" in ''|*[!0-9]*) printf 'SISP_LLM_WARMUP_STEPS must be a non-negative integer\n' >&2; exit 2;; esac
case "$decay_steps" in ''|*[!0-9]*) printf 'SISP_LLM_DECAY_STEPS must be a positive integer\n' >&2; exit 2;; esac
case "$log_every" in ''|*[!0-9]*) printf 'SISP_LLM_LOG_EVERY must be a positive integer\n' >&2; exit 2;; esac
case "$top_k" in ''|*[!0-9]*) printf 'SISP_LLM_TOP_K must be a positive integer\n' >&2; exit 2;; esac
case "$evaluation_count" in ''|*[!0-9]*) printf 'SISP_LLM_EVAL_SEQUENCES must be a positive integer\n' >&2; exit 2;; esac
case "$sampler_seed" in ''|*[!0-9]*) printf 'SISP_LLM_SAMPLER_SEED must be a non-negative integer\n' >&2; exit 2;; esac
case "$observer" in 0|1) : ;; *) printf 'SISP_LLM_OBSERVER must be 0 or 1\n' >&2; exit 2;; esac
case "$observer_full" in 0|1) : ;; *) printf 'SISP_LLM_OBSERVER_FULL must be 0 or 1\n' >&2; exit 2;; esac
case "$functional_eval" in 0|1) : ;; *) printf 'SISP_LLM_FUNCTIONAL_EVAL must be 0 or 1\n' >&2; exit 2;; esac
if [ "$functional_eval" -eq 1 ]; then
  if [ -z "$functional_output" ] || [ ! -d "$functional_output" ]; then
    printf 'SISP_LLM_FUNCTIONAL_OUTPUT must be an existing directory\n' >&2
    exit 2
  fi
  if [ ! -f "$functional_tasks" ]; then
    printf 'SISP_LLM_FUNCTIONAL_TASKS must name an existing task schedule\n' >&2
    exit 2
  fi
  case "$functional_tasks" in *'"'*|*'\'*|*'
'*) printf 'functional task path cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
  case "$functional_output" in *'"'*|*'\'*|*'
'*) printf 'functional output path cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
fi
if [ "$microbatch" -eq 0 ] || [ "$decay_steps" -eq 0 ] || [ "$log_every" -eq 0 ] || [ "$top_k" -eq 0 ] || [ "$evaluation_count" -eq 0 ]; then
  printf 'microbatch, decay steps, log interval, top-k, and evaluation count must be greater than zero\n' >&2
  exit 2
fi
if [ "$warmup_steps" -ge "$decay_steps" ]; then
  printf 'warmup steps must be less than decay steps\n' >&2
  exit 2
fi
if [ -n "$test_corpus" ]; then
  if [ "$steps" -ne 0 ] || [ -z "$resume" ] || [ -n "$save" ]; then
    printf 'final test evaluation requires steps=0, a checkpoint resume, and no save path\n' >&2
    exit 2
  fi
  if [ "$functional_eval" -ne 0 ] || [ "$observer" -ne 0 ]; then
    printf 'final test evaluation cannot be combined with observer or functional evaluation\n' >&2
    exit 2
  fi
fi
case "$corpus_mode" in strict|new) : ;; *) printf 'SISP_LLM_CORPUS_MODE must be strict or new\n' >&2; exit 2;; esac
case "$sampling" in shuffle|sequential) : ;; *) printf 'SISP_LLM_SAMPLING must be shuffle or sequential\n' >&2; exit 2;; esac
if ! awk -v value="$temperature" 'BEGIN {exit !(value ~ /^[0-9]+([.][0-9]+)?$/ && value > 0)}'; then
  printf 'SISP_LLM_TEMPERATURE must be a positive decimal\n' >&2
  exit 2
fi
if ! awk -v value="$top_p" 'BEGIN {exit !(value ~ /^(0[.][0-9]+|1([.]0+)?)$/ && value > 0 && value <= 1)}'; then
  printf 'SISP_LLM_TOP_P must be greater than zero and at most one\n' >&2
  exit 2
fi
if ! awk -v value="$clip_norm" 'BEGIN {exit !(value ~ /^[0-9]+([.][0-9]+)?$/ && value > 0)}'; then
  printf 'SISP_LLM_CLIP_NORM must be a positive decimal\n' >&2
  exit 2
fi
if ! awk -v value="$beta2" 'BEGIN {exit !(value ~ /^0[.][0-9]+$/ && value > 0 && value < 1)}'; then
  printf 'SISP_LLM_BETA2 must be between zero and one\n' >&2
  exit 2
fi
if ! awk -v value="$learning_rate" 'BEGIN {exit !(value ~ /^0[.][0-9]+$/ && value > 0)}'; then
  printf 'SISP_LLM_LEARNING_RATE must be a positive decimal below one\n' >&2
  exit 2
fi
if ! awk -v value="$weight_decay" 'BEGIN {exit !(value ~ /^[0-9]+([.][0-9]+)?$/ && value >= 0)}'; then
  printf 'SISP_LLM_WEIGHT_DECAY must be a non-negative decimal\n' >&2
  exit 2
fi
if ! awk -v value="$min_learning_rate_ratio" 'BEGIN {exit !(value ~ /^0[.][0-9]+$/ && value > 0 && value <= 1)}'; then
  printf 'SISP_LLM_MIN_LR_RATIO must be between zero and one\n' >&2
  exit 2
fi
for input_path in "$corpus" "$validation_corpus" "$vocabulary"; do
  if [ ! -f "$input_path" ]; then
    printf 'required LLM input file not found: %s\n' "$input_path" >&2
    exit 2
  fi
  case "$input_path" in *'"'*|*'\'*|*'
'*) printf 'LLM input paths cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
done
if [ -n "$test_corpus" ]; then
  if [ ! -f "$test_corpus" ]; then
    printf 'SISP_LLM_TEST_CORPUS does not exist: %s\n' "$test_corpus" >&2
    exit 2
  fi
  case "$test_corpus" in *'"'*|*'\'*|*'
'*) printf 'SISP_LLM_TEST_CORPUS cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
fi
for span_path in "$corpus_spans" "$validation_spans"; do
  [ -z "$span_path" ] && continue
  if [ ! -f "$span_path" ]; then
    printf 'required LLM span file not found: %s\n' "$span_path" >&2
    exit 2
  fi
  case "$span_path" in *'"'*|*'\'*|*'
'*) printf 'LLM span paths cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
done
if [ -n "$test_corpus" ] && [ -z "$test_spans" ]; then
  printf 'final test evaluation requires SISP_LLM_TEST_SPANS or a sibling test-spans.lsp\n' >&2
  exit 2
fi
if [ -n "$test_spans" ] && [ ! -f "$test_spans" ]; then
  printf 'required test span file not found: %s\n' "$test_spans" >&2
  exit 2
fi
if [ -n "$prompt_file" ]; then
  if [ ! -f "$prompt_file" ]; then
    printf 'SISP_LLM_PROMPT_FILE does not exist: %s\n' "$prompt_file" >&2
    exit 2
  fi
  case "$prompt_file" in *'"'*|*'\'*|*'
'*) printf 'SISP_LLM_PROMPT_FILE cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
fi

corpus_checksum=$({ cat "$corpus"; [ -z "$corpus_spans" ] || cat "$corpus_spans"; } | cksum | awk '{print $1}')
validation_corpus_checksum=$({ cat "$validation_corpus"; [ -z "$validation_spans" ] || cat "$validation_spans"; } | cksum | awk '{print $1}')
vocabulary_checksum=$(cksum <"$vocabulary" | awk '{print $1}')
test_corpus_checksum=0
if [ -n "$test_corpus" ]; then
  test_corpus_checksum=$({ cat "$test_corpus"; cat "$test_spans"; } | cksum | awk '{print $1}')
fi

observer_full_lisp=nil
if [ "$observer_full" -eq 1 ]; then observer_full_lisp=t; fi

config=$(mktemp /tmp/sisp-matrix-config.XXXXXX)
schedule=$(mktemp /tmp/sisp-matrix-schedule.XXXXXX)
initial_evaluation=$(mktemp /tmp/sisp-matrix-initial-eval.XXXXXX)
final_evaluation=$(mktemp /tmp/sisp-matrix-final-eval.XXXXXX)
generation=$(mktemp /tmp/sisp-matrix-generation.XXXXXX)
save_call=$(mktemp /tmp/sisp-matrix-save.XXXXXX)
errors=$(mktemp /tmp/sisp-matrix-errors.XXXXXX)
stage=
previous=
trap 'rm -f "$config" "$schedule" "$initial_evaluation" "$final_evaluation" "$generation" "$save_call" "$errors"; if [ -n "$stage" ] && [ -d "$stage" ]; then rm -rf "$stage"; fi; if [ -n "$save" ] && [ ! -e "$save" ] && [ -n "$previous" ] && [ -e "$previous" ]; then mv "$previous" "$save"; fi' EXIT HUP INT TERM

if [ -n "$resume" ]; then
  if [ ! -d "$resume" ] || [ ! -f "$resume/state.lsp" ]; then
    printf 'SISP_LLM_RESUME is not a complete checkpoint directory: %s\n' "$resume" >&2
    exit 2
  fi
  set -- "$resume"/part-*.lsp
  if [ ! -e "$1" ]; then
    printf 'SISP_LLM_RESUME contains no parameter parts: %s\n' "$resume" >&2
    exit 2
  fi
  if ! python3 "$checkpoint_integrity" verify "$resume"; then
    printf 'SISP_LLM_RESUME failed checkpoint integrity verification: %s\n' "$resume" >&2
    exit 2
  fi
fi

if [ -n "$save" ]; then
  case "$save" in *'"'*|*'\'*|*'
'*) printf 'SISP_LLM_SAVE cannot contain quotes, backslashes, or newlines\n' >&2; exit 2;; esac
  mkdir -p "$(dirname "$save")"
  stage="${save}.tmp.$$"
  if ! mkdir "$stage"; then
    printf 'cannot create checkpoint staging directory: %s\n' "$stage" >&2
    exit 2
  fi
  awk -v count="$parameter_objects" 'BEGIN {
    print "(matrix-checkpoint-validate-save)"
    print "(print (list (quote checkpoint-save-start) *matrix-checkpoint-output* (quote step) *matrix-step*))"
    for (i = 0; i < count; i++) {
      print "(matrix-checkpoint-save-component *matrix-checkpoint-output* " i " (quote data))"
      print "(matrix-checkpoint-save-component *matrix-checkpoint-output* " i " (quote m))"
      print "(matrix-checkpoint-save-component *matrix-checkpoint-output* " i " (quote v))"
    }
    print "(matrix-checkpoint-save-finish *matrix-checkpoint-output*)"
  }' >"$save_call"
fi

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
  "(define *matrix-generation-count* $tokens)" \
  "(define *matrix-requested-steps* $steps)" \
  "(define *matrix-prompt-path* \"$prompt_file\")" \
  "(define *matrix-corpus-path* \"$corpus\")" \
  "(define *matrix-validation-corpus-path* \"$validation_corpus\")" \
  "(define *matrix-test-corpus-path* \"$test_corpus\")" \
  "(define *matrix-test-corpus-checksum* $test_corpus_checksum)" \
  "(define *matrix-training-spans* nil)" \
  "(define *matrix-validation-spans* nil)" \
  "(define *matrix-test-spans* nil)" \
  "(define *matrix-vocabulary-path* \"$vocabulary\")" \
  "(define *matrix-corpus-checksum* $corpus_checksum)" \
  "(define *matrix-validation-corpus-checksum* $validation_corpus_checksum)" \
  "(define *matrix-vocabulary-checksum* $vocabulary_checksum)" \
  "(define *matrix-corpus-mode* (quote $corpus_mode))" \
  "(define *matrix-sampling-mode* (quote $sampling))" \
  "(define *matrix-sampling-mode-name* \"$sampling\")" \
  "(define *matrix-sampler-seed* $sampler_seed)" \
  "(define *matrix-microbatch* $microbatch)" \
  "(define *matrix-warmup-steps* $warmup_steps)" \
  "(define *matrix-decay-steps* $decay_steps)" \
  "(define *matrix-min-learning-rate-ratio* $min_learning_rate_ratio)" \
  "(define *matrix-clip-norm* $clip_norm)" \
  "(define *matrix-beta2* $beta2)" \
  "(define *matrix-log-every* $log_every)" \
  "(define *matrix-temperature* $temperature)" \
  "(define *matrix-top-k* $top_k)" \
  "(define *matrix-top-p* $top_p)" \
  "(define *matrix-observer-full* $observer_full_lisp)" \
  "(define *matrix-functional-output* \"$functional_output\")" \
  "(define *matrix-checkpoint-output* \"$stage\")" >"$config"

awk -v steps="$steps" -v microbatch="$microbatch" 'BEGIN {
  for (i = 0; i < steps; i++) {
    print "(matrix-training-begin)"
    for (j = 0; j < microbatch; j++) print "(matrix-training-accumulate)"
    print "(matrix-training-apply)"
  }
}' >"$schedule"
if [ -n "$test_corpus" ] || [ -n "$resume" ]; then
  printf '%s\n' \
    '(define *matrix-initial-training-loss* 0.0)' \
    '(define *matrix-initial-loss* 0.0)' >"$initial_evaluation"
else
awk -v count="$evaluation_count" 'BEGIN {
  print "(matrix-evaluation-reset)"
  for (i = 0; i < count; i++) print "(matrix-evaluation-accumulate-training " i ")"
  print "(define *matrix-initial-training-loss* (matrix-evaluation-mean))"
  print "(matrix-evaluation-reset)"
  for (i = 0; i < count; i++) print "(matrix-evaluation-accumulate-validation " i ")"
  print "(define *matrix-initial-loss* (matrix-evaluation-mean))"
  print "(print (list (quote initial-training-nll) *matrix-initial-training-loss* (quote initial-validation-nll) *matrix-initial-loss* (quote initial-validation-bits-per-byte) (/ *matrix-initial-loss* (log 2.0)) (quote initial-validation-perplexity) (exp *matrix-initial-loss*)))"
}' >"$initial_evaluation"
fi
awk -v count="$evaluation_count" 'BEGIN {
  print "(matrix-evaluation-reset)"
  for (i = 0; i < count; i++) print "(matrix-evaluation-accumulate-training " i ")"
  print "(define *matrix-final-training-loss* (matrix-evaluation-mean))"
  print "(matrix-evaluation-reset)"
  for (i = 0; i < count; i++) print "(matrix-evaluation-accumulate-validation " i ")"
  print "(define *matrix-final-loss* (matrix-evaluation-mean))"
}' >"$final_evaluation"
awk -v tokens="$tokens" 'BEGIN {
  for (i = 0; i < tokens; i++) print "(matrix-generation-step)"
}' >"$generation"

set -- src/sisp \
  ml/llm/math.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-checkpoint.lsp \
  "$config"

if [ -n "$corpus_spans" ]; then set -- "$@" "$corpus_spans"; fi
if [ -n "$validation_spans" ]; then set -- "$@" "$validation_spans"; fi
set -- "$@" ml/llm/matrix-corpus.lsp

if [ -n "$resume" ]; then
  for checkpoint_part in "$resume"/part-*.lsp; do
    set -- "$@" "$checkpoint_part"
  done
  set -- "$@" "$resume/state.lsp"
fi

if [ -n "$test_corpus" ]; then
  set -- "$@" "$test_spans" ml/llm/matrix-test-setup.lsp
fi

set -- "$@" \
  ml/llm/matrix-corpus-start.lsp \
  "$initial_evaluation" \
  "$schedule" \
  "$final_evaluation" \
  ml/llm/matrix-corpus-final.lsp \
  "$save_call" \
  ml/llm/matrix-corpus-generate.lsp \
  "$generation" \
  ml/llm/matrix-corpus-report.lsp

if [ -n "$test_corpus" ]; then
  set -- "$@" ml/llm/matrix-test-report.lsp
fi

if [ "$observer" -eq 1 ]; then
  set -- "$@" ml/llm/observer.lsp ml/llm/observer-run.lsp
fi
if [ "$functional_eval" -eq 1 ]; then
  set -- "$@" ml/llm/functional-eval.lsp "$functional_tasks"
fi

if ! "$@" </dev/null 2>"$errors"; then
  cat "$errors" >&2
  exit 1
fi
if [ -s "$errors" ]; then
  cat "$errors" >&2
  exit 1
fi

if [ -n "$save" ]; then
  if [ ! -f "$stage/state.lsp" ]; then
    printf 'checkpoint did not produce a state file\n' >&2
    exit 1
  fi
  if [ ! -f "$stage/metrics.txt" ]; then
    printf 'checkpoint did not produce finite metrics\n' >&2
    exit 1
  fi
  expected_parts=$((parameter_objects * 3))
  actual_parts=$(find "$stage" -type f -name 'part-*.lsp' | wc -l | tr -d ' ')
  if [ "$actual_parts" -ne "$expected_parts" ]; then
    printf 'checkpoint has %s parts; expected %s\n' "$actual_parts" "$expected_parts" >&2
    exit 1
  fi
  if ! python3 "$checkpoint_integrity" create "$stage"; then
    printf 'could not create checkpoint integrity manifest\n' >&2
    exit 1
  fi
  if [ -e "$save" ]; then
    previous="${save}.previous"
    rm -rf "$previous"
    mv "$save" "$previous"
  fi
  if ! mv "$stage" "$save"; then
    if [ -e "$previous" ] && [ ! -e "$save" ]; then mv "$previous" "$save"; fi
    printf 'could not publish checkpoint: %s\n' "$save" >&2
    exit 1
  fi
  stage=
  printf 'checkpoint published: %s\n' "$save"
  if [ -d "$previous" ]; then
    printf 'previous checkpoint retained: %s\n' "$previous"
  fi
fi

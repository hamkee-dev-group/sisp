#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

checkpoint_integrity=ml/llm/checkpoint-integrity.py

profile=${SISP_LLM_PROFILE:-practical}
case "$profile" in
  small) default_chunk_steps=500 default_eval_sequences=32 ;;
  practical) default_chunk_steps=500 default_eval_sequences=64 ;;
  code) default_chunk_steps=250 default_eval_sequences=256 ;;
  code-long) default_chunk_steps=125 default_eval_sequences=128 ;;
  code-plus) default_chunk_steps=250 default_eval_sequences=256 ;;
  research) default_chunk_steps=100 default_eval_sequences=128 ;;
  large) default_chunk_steps=25 default_eval_sequences=64 ;;
  *) printf 'unknown SISP_LLM_PROFILE: %s\n' "$profile" >&2; exit 2 ;;
esac
chunk_steps=${SISP_LLM_CHUNK_STEPS:-$default_chunk_steps}
eval_sequences=${SISP_LLM_EVAL_SEQUENCES:-$default_eval_sequences}
checkpoint=${SISP_LLM_CHECKPOINT:-ml/llm/checkpoints/$profile}
log=${SISP_LLM_LOG:-${checkpoint}.log}
max_chunks=${SISP_LLM_MAX_CHUNKS:-0}
max_hours=${SISP_LLM_MAX_HOURS:-0}
max_seconds=${SISP_LLM_MAX_SECONDS:-0}
keep_best=${SISP_LLM_KEEP_BEST:-1}
best_checkpoint=${SISP_LLM_BEST_CHECKPOINT:-${checkpoint}.best}
patience_chunks=${SISP_LLM_PATIENCE_CHUNKS:-0}

case "$chunk_steps" in ''|*[!0-9]*) printf 'SISP_LLM_CHUNK_STEPS must be a positive integer\n' >&2; exit 2;; esac
case "$eval_sequences" in ''|*[!0-9]*) printf 'SISP_LLM_EVAL_SEQUENCES must be a positive integer\n' >&2; exit 2;; esac
case "$max_chunks" in ''|*[!0-9]*) printf 'SISP_LLM_MAX_CHUNKS must be a non-negative integer\n' >&2; exit 2;; esac
case "$max_hours" in ''|*[!0-9]*) printf 'SISP_LLM_MAX_HOURS must be a non-negative integer\n' >&2; exit 2;; esac
case "$max_seconds" in ''|*[!0-9]*) printf 'SISP_LLM_MAX_SECONDS must be a non-negative integer\n' >&2; exit 2;; esac
case "$keep_best" in 0|1) : ;; *) printf 'SISP_LLM_KEEP_BEST must be 0 or 1\n' >&2; exit 2;; esac
case "$patience_chunks" in ''|*[!0-9]*) printf 'SISP_LLM_PATIENCE_CHUNKS must be a non-negative integer\n' >&2; exit 2;; esac
if [ "$chunk_steps" -eq 0 ] || [ "$eval_sequences" -eq 0 ]; then
  printf 'SISP_LLM_CHUNK_STEPS and SISP_LLM_EVAL_SEQUENCES must be greater than zero\n' >&2
  exit 2
fi

mkdir -p "$(dirname "$checkpoint")" "$(dirname "$log")"
lock="${checkpoint}.lock"
if ! mkdir "$lock" 2>/dev/null; then
  printf 'another continuous trainer owns lock: %s\n' "$lock" >&2
  exit 2
fi
printf '%s\n' "pid=$$ host=$(hostname) started=$(date -u '+%Y-%m-%dT%H:%M:%SZ')" >"$lock/owner"
best_stage=
trap 'if [ -n "$best_stage" ] && [ -d "$best_stage" ]; then rm -rf "$best_stage"; fi; rm -rf "$lock"' EXIT HUP INT TERM
if [ ! -d "$best_checkpoint" ] && [ -d "${best_checkpoint}.previous" ]; then
  mv "${best_checkpoint}.previous" "$best_checkpoint"
fi
if [ "$max_seconds" -gt 0 ]; then
  time_limit=$max_seconds
else
  time_limit=$((max_hours * 3600))
fi
started_at=$(date +%s)
chunk=0
unimproved=0
while :; do
  chunk=$((chunk + 1))
  printf '%s\n' "$(date -u '+%Y-%m-%dT%H:%M:%SZ') starting chunk $chunk" | tee -a "$log"

  resume=
  if [ -e "$checkpoint" ]; then
    if python3 "$checkpoint_integrity" verify "$checkpoint" >>"$log" 2>&1; then
      resume=$checkpoint
    elif [ -e "${checkpoint}.previous" ] && \
         python3 "$checkpoint_integrity" verify "${checkpoint}.previous" >>"$log" 2>&1; then
      corrupt="${checkpoint}.corrupt.$(date +%s)"
      if ! mv "$checkpoint" "$corrupt"; then
        printf '%s\n' "could not quarantine invalid checkpoint $checkpoint" | tee -a "$log"
        exit 1
      fi
      printf '%s\n' "invalid current checkpoint quarantined at $corrupt; recovering previous" | tee -a "$log"
      resume=${checkpoint}.previous
    else
      printf '%s\n' 'checkpoint artifacts exist but no verified recovery checkpoint is available' | tee -a "$log"
      exit 1
    fi
  elif [ -e "${checkpoint}.previous" ]; then
    if python3 "$checkpoint_integrity" verify "${checkpoint}.previous" >>"$log" 2>&1; then
      resume=${checkpoint}.previous
    else
      printf '%s\n' 'previous checkpoint exists but fails integrity verification' | tee -a "$log"
      exit 1
    fi
  fi

  if [ -n "$resume" ]; then
    SISP_LLM_PROFILE="$profile" \
    SISP_LLM_STEPS="$chunk_steps" \
    SISP_LLM_TOKENS=1 \
    SISP_LLM_EVAL_SEQUENCES="$eval_sequences" \
    SISP_LLM_RESUME="$resume" \
    SISP_LLM_SAVE="$checkpoint" \
      ml/llm/run-matrix-corpus.sh >>"$log" 2>&1
  else
    SISP_LLM_PROFILE="$profile" \
    SISP_LLM_STEPS="$chunk_steps" \
    SISP_LLM_TOKENS=1 \
    SISP_LLM_EVAL_SEQUENCES="$eval_sequences" \
    SISP_LLM_SAVE="$checkpoint" \
      ml/llm/run-matrix-corpus.sh >>"$log" 2>&1
  fi

  printf '%s\n' "$(date -u '+%Y-%m-%dT%H:%M:%SZ') completed chunk $chunk" | tee -a "$log"
  if [ "$keep_best" -eq 1 ] && [ -f "$checkpoint/metrics.txt" ]; then
    current_nll=$(awk '{for (i=1; i<NF; i++) if ($i == "validation-nll") {print $(i+1); exit}}' "$checkpoint/metrics.txt")
    if ! awk -v value="$current_nll" 'BEGIN {exit !(value ~ /^[0-9]+([.][0-9]+)?$/ && value < 1000000)}'; then
      printf '%s\n' "invalid validation metric; refusing best-checkpoint update" | tee -a "$log"
      exit 1
    fi
    best_nll=
    if [ -f "$best_checkpoint/metrics.txt" ]; then
      best_nll=$(awk '{for (i=1; i<NF; i++) if ($i == "validation-nll") {print $(i+1); exit}}' "$best_checkpoint/metrics.txt")
    fi
    if [ -n "$current_nll" ] && { [ -z "$best_nll" ] || awk -v current="$current_nll" -v best="$best_nll" 'BEGIN {exit !(current < best)}'; }; then
      best_stage="${best_checkpoint}.tmp.$$"
      rm -rf "$best_stage"
      cp -R "$checkpoint" "$best_stage"
      rm -rf "${best_checkpoint}.previous"
      if [ -d "$best_checkpoint" ]; then mv "$best_checkpoint" "${best_checkpoint}.previous"; fi
      if ! mv "$best_stage" "$best_checkpoint"; then
        if [ ! -d "$best_checkpoint" ] && [ -d "${best_checkpoint}.previous" ]; then
          mv "${best_checkpoint}.previous" "$best_checkpoint"
        fi
        printf '%s\n' "could not publish best checkpoint" | tee -a "$log"
        exit 1
      fi
      best_stage=
      rm -rf "${best_checkpoint}.previous"
      unimproved=0
      printf '%s\n' "$(date -u '+%Y-%m-%dT%H:%M:%SZ') new best validation NLL $current_nll" | tee -a "$log"
    else
      unimproved=$((unimproved + 1))
    fi
  fi
  if [ "$max_chunks" -gt 0 ] && [ "$chunk" -ge "$max_chunks" ]; then
    break
  fi
  if [ "$patience_chunks" -gt 0 ] && [ "$unimproved" -ge "$patience_chunks" ]; then
    printf '%s\n' "stopping after $unimproved chunks without validation improvement" | tee -a "$log"
    break
  fi
  if [ "$time_limit" -gt 0 ] && [ $(($(date +%s) - started_at)) -ge "$time_limit" ]; then
    printf '%s\n' "time limit reached after completed chunk $chunk" | tee -a "$log"
    break
  fi
done

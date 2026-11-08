#!/bin/sh
# Report the untouched prepared test split once, without training or saving.
set -eu

cd "$(dirname "$0")/../.."

if [ -z "${SISP_LLM_RESUME:-}" ]; then
  printf 'SISP_LLM_RESUME is required for final test evaluation\n' >&2
  exit 2
fi
if [ -z "${SISP_LLM_TEST_CORPUS:-}" ]; then
  printf 'SISP_LLM_TEST_CORPUS is required for final test evaluation\n' >&2
  exit 2
fi

SISP_LLM_STEPS=0 \
SISP_LLM_TOKENS=${SISP_LLM_TOKENS:-1} \
SISP_LLM_EVAL_SEQUENCES=${SISP_LLM_EVAL_SEQUENCES:-256} \
SISP_LLM_CORPUS_MODE=strict \
SISP_LLM_SAVE= \
  exec ml/llm/run-matrix-corpus.sh

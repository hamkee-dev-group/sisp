#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

work=$(mktemp -d /tmp/sisp-matrix-checkpoint-test.XXXXXX)
trap 'rm -rf "$work"' EXIT HUP INT TERM

# Two steps, checkpoint, restart, and one more step must be byte-for-byte the
# same as an uninterrupted three-step run.  This covers weights, AdamW moments,
# step bias correction, and the next corpus-window position.
SISP_LLM_PROFILE=small SISP_LLM_STEPS=2 SISP_LLM_TOKENS=1 \
  SISP_LLM_SAVE="$work/after-two" \
  ml/llm/run-matrix-corpus.sh >/dev/null

SISP_LLM_PROFILE=small SISP_LLM_STEPS=1 SISP_LLM_TOKENS=1 \
  SISP_LLM_RESUME="$work/after-two" SISP_LLM_SAVE="$work/resumed-three" \
  ml/llm/run-matrix-corpus.sh >/dev/null

SISP_LLM_PROFILE=small SISP_LLM_STEPS=3 SISP_LLM_TOKENS=1 \
  SISP_LLM_SAVE="$work/direct-three" \
  ml/llm/run-matrix-corpus.sh >/dev/null

if ! diff -r "$work/resumed-three" "$work/direct-three" >/dev/null; then
  printf '%s\n' 'FAIL: resumed checkpoint differs from uninterrupted training'
  exit 1
fi

if SISP_LLM_PROFILE=practical SISP_LLM_STEPS=1 SISP_LLM_TOKENS=1 \
    SISP_LLM_RESUME="$work/after-two" \
    ml/llm/run-matrix-corpus.sh >/dev/null 2>&1; then
  printf '%s\n' 'FAIL: incompatible profile checkpoint was accepted'
  exit 1
fi

if SISP_LLM_PROFILE=small SISP_LLM_STEPS=1 SISP_LLM_TOKENS=1 \
    SISP_LLM_BETA2=0.99 SISP_LLM_RESUME="$work/after-two" \
    ml/llm/run-matrix-corpus.sh >/dev/null 2>&1; then
  printf '%s\n' 'FAIL: incompatible optimizer schedule was accepted'
  exit 1
fi

cp -R "$work/after-two" "$work/duplicate-part"
cp "$work/duplicate-part/part-0-data.lsp" \
   "$work/duplicate-part/part-1-data.lsp"
if SISP_LLM_PROFILE=small SISP_LLM_STEPS=1 SISP_LLM_TOKENS=1 \
    SISP_LLM_RESUME="$work/duplicate-part" \
    ml/llm/run-matrix-corpus.sh >/dev/null 2>&1; then
  printf '%s\n' 'FAIL: duplicate logical checkpoint part was accepted'
  exit 1
fi

cp -R "$work/after-two" "$work/nonfinite-part"
sed 's/(list [^ ]*/(list 10000000000000000000000000000000.0/' \
  "$work/nonfinite-part/part-0-data.lsp" >"$work/nonfinite-part/corrupt.lsp"
mv "$work/nonfinite-part/corrupt.lsp" "$work/nonfinite-part/part-0-data.lsp"
if SISP_LLM_PROFILE=small SISP_LLM_STEPS=0 SISP_LLM_TOKENS=1 \
    SISP_LLM_RESUME="$work/nonfinite-part" \
    ml/llm/run-matrix-corpus.sh >/dev/null 2>&1; then
  printf '%s\n' 'FAIL: non-finite checkpoint state was accepted'
  exit 1
fi

# Continuous training may recover a verified previous publication, but must
# quarantine an invalid current directory instead of loading it or starting over.
cp -R "$work/after-two" "$work/recovery.previous"
cp -R "$work/after-two" "$work/recovery"
printf x >>"$work/recovery/part-0-data.lsp"
SISP_LLM_PROFILE=small SISP_LLM_CHECKPOINT="$work/recovery" \
  SISP_LLM_LOG="$work/recovery.log" SISP_LLM_MAX_CHUNKS=1 \
  SISP_LLM_CHUNK_STEPS=1 SISP_LLM_KEEP_BEST=0 \
  ml/llm/run-matrix-forever.sh >/dev/null
if ! python3 ml/llm/checkpoint-integrity.py verify "$work/recovery" || \
   ! grep 'step 3 ' "$work/recovery/metrics.txt" >/dev/null || \
   ! grep 'recovering previous' "$work/recovery.log" >/dev/null; then
  printf '%s\n' 'FAIL: continuous trainer did not recover the verified previous checkpoint'
  exit 1
fi

# Prepared-corpus spans are part of the deterministic sampling contract.  A
# stopped/resumed run must match directly, and changing only the span metadata
# must invalidate strict resume even when corpus text is unchanged.
python3 ml/llm/prepare-corpus.py \
  algorithms/ai/kmeans.lsp \
  algorithms/ai/perceptron.lsp \
  algorithms/ai/decision-tree.lsp \
  --output "$work/corpus" --comments balanced --unsupported drop >/dev/null
SISP_LLM_PROFILE=small SISP_LLM_STEPS=2 SISP_LLM_TOKENS=1 \
  SISP_LLM_CORPUS="$work/corpus/train.txt" \
  SISP_LLM_VALID_CORPUS="$work/corpus/valid.txt" \
  SISP_LLM_SAVE="$work/span-after-two" \
  ml/llm/run-matrix-corpus.sh >/dev/null
SISP_LLM_PROFILE=small SISP_LLM_STEPS=1 SISP_LLM_TOKENS=1 \
  SISP_LLM_CORPUS="$work/corpus/train.txt" \
  SISP_LLM_VALID_CORPUS="$work/corpus/valid.txt" \
  SISP_LLM_RESUME="$work/span-after-two" SISP_LLM_SAVE="$work/span-resumed" \
  ml/llm/run-matrix-corpus.sh >/dev/null
SISP_LLM_PROFILE=small SISP_LLM_STEPS=3 SISP_LLM_TOKENS=1 \
  SISP_LLM_CORPUS="$work/corpus/train.txt" \
  SISP_LLM_VALID_CORPUS="$work/corpus/valid.txt" \
  SISP_LLM_SAVE="$work/span-direct" \
  ml/llm/run-matrix-corpus.sh >/dev/null
if ! diff -r "$work/span-resumed" "$work/span-direct" >/dev/null; then
  printf '%s\n' 'FAIL: span-aware resume differs from uninterrupted training'
  exit 1
fi
sed 's/\[0 /[1 /' "$work/corpus/train-spans.lsp" >"$work/changed-spans.lsp"
if SISP_LLM_PROFILE=small SISP_LLM_STEPS=0 SISP_LLM_TOKENS=1 \
    SISP_LLM_CORPUS="$work/corpus/train.txt" \
    SISP_LLM_VALID_CORPUS="$work/corpus/valid.txt" \
    SISP_LLM_CORPUS_SPANS="$work/changed-spans.lsp" \
    SISP_LLM_RESUME="$work/span-after-two" \
    ml/llm/run-matrix-corpus.sh >/dev/null 2>&1; then
  printf '%s\n' 'FAIL: changed corpus spans were accepted by strict resume'
  exit 1
fi

# The untouched test split is evaluated only in inference mode and must not
# modify the checkpoint used for the report.
before_test=$(cksum "$work/span-after-two/integrity.json")
SISP_LLM_PROFILE=small SISP_LLM_STEPS=0 SISP_LLM_TOKENS=1 \
  SISP_LLM_CORPUS="$work/corpus/train.txt" \
  SISP_LLM_VALID_CORPUS="$work/corpus/valid.txt" \
  SISP_LLM_RESUME="$work/span-after-two" \
  SISP_LLM_TEST_CORPUS="$work/corpus/test.txt" \
  SISP_LLM_TEST_SPANS="$work/corpus/test-spans.lsp" \
  SISP_LLM_EVAL_SEQUENCES=2 \
  ml/llm/run-matrix-test.sh >"$work/test-output"
after_test=$(cksum "$work/span-after-two/integrity.json")
if [ "$before_test" != "$after_test" ] || \
   ! grep 'final-test-nll' "$work/test-output" >/dev/null; then
  printf '%s\n' 'FAIL: final test evaluation changed checkpoint or produced no metric'
  exit 1
fi

printf '%s\n' '(pass "exact checkpoint resume matches uninterrupted training")'
printf '%s\n' '(pass "incompatible checkpoint is rejected")'
printf '%s\n' '(pass "incompatible optimizer schedule is rejected")'
printf '%s\n' '(pass "duplicate logical checkpoint part is rejected")'
printf '%s\n' '(pass "non-finite checkpoint state is rejected")'
printf '%s\n' '(pass "span-aware checkpoint resume is exact")'
printf '%s\n' '(pass "changed span metadata is rejected")'
printf '%s\n' '(pass "final test evaluation is read-only")'
printf '%s\n' '(pass "continuous trainer recovers verified previous checkpoint")'
printf '%s\n' '(tests passed 9 failed 0)'

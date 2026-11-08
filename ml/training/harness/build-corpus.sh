#!/bin/sh
# Validate synthetic units and build reproducible local train/valid/test files.
set -eu

ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
OUTPUT=${1:-$ROOT/ml/llm/data/local/sisp-v1}
CHECK="$ROOT/ml/training/harness/check-oracles.sh"
PREPARE="$ROOT/ml/llm/prepare-corpus.py"
FIM_RATE=${SISP_LLM_FIM_RATE:-0}
FIM_SEED=${SISP_LLM_FIM_SEED:-314159}

cd "$ROOT"
if [ ! -x src/sisp ]; then
  printf 'build the interpreter first: make -C src\n' >&2
  exit 2
fi

# Agent trajectories are policy text rather than standalone .lsp programs.
# Validate their exact schema here; the executable agent integration suite is
# intentionally kept in ml/llm/run-all.sh and run-protocol-tests.sh so a
# busy training machine cannot prevent deterministic corpus assembly.
ml/agent/run-protocol-tests.sh --static

# Every authored training unit must execute and satisfy its oracles.
for directory in ml/training/seed ml/training/gen; do
  [ -d "$directory" ] || continue
  for file in "$directory"/*.lsp; do
    [ -f "$file" ] || continue
    if ! sh "$CHECK" "$file" >/dev/null; then
      printf 'oracle validation failed: %s\n' "$file" >&2
      exit 1
    fi
    if [ "$directory" = ml/training/gen ]; then
      annotations=$(grep -cE ';[[:space:]]*=>' "$file" || true)
      checkable=$(grep -cE '^\(.*\)[[:space:]]*;[[:space:]]*=>' "$file" || true)
      if [ "$annotations" -lt 1 ] || [ "$annotations" -gt 3 ] || [ "$annotations" -ne "$checkable" ]; then
        printf 'generated oracle shape invalid: %s (%s/%s checkable)\n' \
          "$file" "$checkable" "$annotations" >&2
        exit 1
      fi
    fi
  done
done

# The reference programs under examples/, projects/, tests/, and src/prelude.lsp
# are authoring inputs only. Their compact, reviewed distillations live in seed/
# and gen/; admitting the originals here would duplicate patterns and train on
# large standalone programs rather than the intended completion lessons.
set --
for file in ml/training/seed/*.lsp; do
  [ -f "$file" ] || continue
  # Distillations of the frozen benchmark expose its private checks and exact
  # answers under new filenames; path-level exclusion of ml/llm/eval is
  # therefore insufficient.
  case "$file" in
    ml/training/seed/dk-llm-eval-*|\
    ml/training/seed/dk-llm-functional-eval*) continue ;;
  esac
  set -- "$@" "$file"
done
if [ -d ml/training/gen ]; then
  for file in ml/training/gen/*.lsp; do
    [ -f "$file" ] && set -- "$@" "$file"
  done
fi
for file in ml/agent/protocol/train/*.txt; do
  [ -f "$file" ] && set -- "$@" "$file"
done

python3 "$PREPARE" "$@" \
  --output "$OUTPUT" \
  --suffix .txt \
  --comments balanced \
  --unsupported drop \
  --fim-rate "$FIM_RATE" \
  --fim-seed "$FIM_SEED" \
  --force-train-prefix ml/agent/protocol/train/

if grep -E '"path": "ml/training/seed/dk-llm-(eval-|functional-eval)' \
    "$OUTPUT/manifest.jsonl" >/dev/null; then
  printf 'distilled functional evaluation leaked into corpus manifest\n' >&2
  exit 1
fi

printf 'corpus written to %s\n' "$OUTPUT"
cat "$OUTPUT/report.json"

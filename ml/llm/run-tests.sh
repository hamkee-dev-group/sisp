#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

run_and_check() {
  output=$("$@" 2>&1)
  printf '%s\n' "$output"
  if printf '%s\n' "$output" | grep '(fail ' >/dev/null; then
    return 1
  fi
  if ! printf '%s\n' "$output" | grep 'failed 0' >/dev/null; then
    return 1
  fi
}

run_and_check src/sisp ml/llm/autograd.lsp </dev/null

run_and_check src/sisp \
  ml/llm/engine.lsp \
  ml/llm/math.lsp \
  ml/llm/tensor.lsp \
  ml/llm/optim.lsp \
  ml/llm/data.lsp \
  ml/llm/nn.lsp \
  ml/llm/model.lsp \
  ml/llm/metrics.lsp \
  ml/llm/core-tests.lsp \
  </dev/null

run_and_check src/sisp \
  ml/llm/math.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-checkpoint.lsp \
  ml/llm/posttrain.lsp \
  ml/llm/posttrain-tests.lsp \
  </dev/null

run_and_check src/sisp \
  ml/llm/math.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-checkpoint.lsp \
  ml/llm/posttrain.lsp \
  ml/llm/posttrain-session.lsp \
  ml/llm/posttrain-native-tests.lsp \
  </dev/null

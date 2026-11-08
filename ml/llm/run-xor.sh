#!/bin/sh
set -eu
cd "$(dirname "$0")/../.."
exec src/sisp \
  ml/llm/engine.lsp \
  ml/llm/math.lsp \
  ml/llm/tensor.lsp \
  ml/llm/optim.lsp \
  ml/llm/nn.lsp \
  ml/llm/metrics.lsp \
  ml/llm/mlp-xor.lsp \
  ml/llm/mlp-xor-train.lsp \
  ml/llm/mlp-xor-report.lsp \
  </dev/null

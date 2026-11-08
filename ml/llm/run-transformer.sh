#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

exec src/sisp \
  ml/llm/engine.lsp \
  ml/llm/math.lsp \
  ml/llm/tensor.lsp \
  ml/llm/optim.lsp \
  ml/llm/data.lsp \
  ml/llm/nn.lsp \
  ml/llm/model.lsp \
  ml/llm/metrics.lsp \
  ml/llm/transformer.lsp \
  ml/llm/transformer-train.lsp \
  ml/llm/transformer-report.lsp \
  </dev/null

#!/bin/sh
set -eu
cd "$(dirname "$0")/../.."
exec src/sisp \
  ml/llm/engine.lsp \
  ml/llm/math.lsp \
  ml/llm/tensor.lsp \
  ml/llm/optim.lsp \
  ml/llm/metrics.lsp \
  ml/llm/reinforce.lsp \
  ml/llm/reinforce-train.lsp \
  ml/llm/reinforce-report.lsp \
  </dev/null

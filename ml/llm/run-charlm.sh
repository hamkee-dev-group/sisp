#!/bin/sh
set -eu
cd "$(dirname "$0")/../.."
exec src/sisp \
  ml/llm/engine.lsp \
  ml/llm/math.lsp \
  ml/llm/tensor.lsp \
  ml/llm/optim.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/charlm.lsp \
  ml/llm/charlm-train.lsp \
  ml/llm/charlm-report.lsp \
  </dev/null

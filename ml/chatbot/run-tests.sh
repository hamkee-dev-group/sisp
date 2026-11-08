#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

src/sisp \
  ml/llm/math.lsp \
  ml/llm/data.lsp \
  ml/llm/metrics.lsp \
  ml/llm/matrix-engine.lsp \
  ml/llm/matrix-nn.lsp \
  ml/llm/matrix-model.lsp \
  ml/llm/matrix-checkpoint.lsp \
  ml/chatbot/chatbot-test-setup.lsp \
  ml/chatbot/chatbot.lsp \
  ml/agent/agent.lsp \
  ml/chatbot/chatbot-tests.lsp </dev/null

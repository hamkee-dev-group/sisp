#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."

# Require a checkpoint unless explicitly testing the random smoke model.
if [ -z "${SISP_LLM_RESUME:-}" ] && [ "${SISP_LLM_ALLOW_RANDOM:-0}" != 1 ]; then
  printf 'SISP_LLM_RESUME is required for native policy evaluation\n' >&2
  exit 2
fi

work=$(mktemp -d "${TMPDIR:-/tmp}/sisp-agent-policy-eval.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM

# Prove that fixture parsing, the agent runtime, and SISP execution work before
# treating malformed model output as an experimental score rather than an
# infrastructure failure.
ml/agent/run-protocol-tests.sh >/dev/null
python3 ml/agent/agent-policy-eval.py prepare "$work"

SISP_LLM_STEPS=0 \
SISP_LLM_TOKENS=${SISP_LLM_TOKENS:-1} \
SISP_LLM_FUNCTIONAL_EVAL=1 \
SISP_LLM_FUNCTIONAL_OUTPUT="$work/candidates" \
SISP_LLM_FUNCTIONAL_TASKS="$work/tasks.lsp" \
ml/llm/run-matrix-corpus.sh

python3 ml/agent/agent-policy-eval.py score "$work" \
  --minimum-semantic "${SISP_AGENT_POLICY_MIN_SEMANTIC:-1}"

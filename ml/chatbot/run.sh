#!/bin/sh
# Launch the native chatbot with a local checkpoint bootstrap.
set -eu

cd "$(dirname "$0")/../.."

command=${1:-chat}
if [ "$#" -gt 0 ]; then shift; fi

case "$command" in
  chat|native)
    [ -f ml/chatbot/local/chatbot.lsp ] || {
      printf 'native chatbot is missing; build/export a local checkpoint first\n' >&2
      exit 2
    }
    exec src/sisp ml/chatbot/local/chatbot.lsp "$@" ;;
  help|-h|--help)
    printf '%s\n' \
      'usage: ml/chatbot/run.sh [chat|native] [SISP FILES...]' \
      'Loads ml/chatbot/local/chatbot.lsp and opens the native SISP REPL.'
    ;;
  *) printf 'unknown chatbot command: %s\n' "$command" >&2; exit 2 ;;
esac

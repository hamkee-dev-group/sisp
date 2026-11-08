#!/bin/sh
# Quantum computer + OS runner.
#   examples/quantum/run.sh          # narrated demo
#   examples/quantum/run.sh tests    # algorithm test suite
# SISP opens a REPL after its file args, so stdin is /dev/null.
set -e
cd "$(dirname "$0")/../.."
SISP=./src/sisp
CORE="examples/quantum/qvm.lsp examples/quantum/algorithms.lsp examples/quantum/qos.lsp"
case "${1:-demo}" in
  tests) exec $SISP $CORE examples/quantum/tests.lsp < /dev/null ;;
  demo)  exec $SISP $CORE examples/quantum/demo.lsp  < /dev/null ;;
  *)     echo "usage: $0 {demo|tests}"; exit 1 ;;
esac

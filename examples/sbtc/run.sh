#!/bin/sh
# sBTC — runner and persistent-ledger CLI.
#
# Demo / tests:
#   examples/sbtc/run.sh                 # narrated end-to-end demo
#   examples/sbtc/run.sh tests           # self-test suite
#
# Persistent ledger (state saved to $SBTC_DATA, default chain.dat):
#   examples/sbtc/run.sh new     <who>          # show a wallet address
#   examples/sbtc/run.sh init    <miner>        # create genesis
#   examples/sbtc/run.sh faucet  <who>          # deposit fictional coins
#   examples/sbtc/run.sh mine    <miner>        # mine a block
#   examples/sbtc/run.sh send    <from> <to> <n># queue a payment
#   examples/sbtc/run.sh balance <who>          # show a balance
#   examples/sbtc/run.sh status                 # chain summary
#   examples/sbtc/run.sh log                    # block history
#
# Each ledger command loads state from disk, does one action, and
# saves — so the chain really grows and every action is recorded.
# SISP opens a REPL after its file args, so stdin is /dev/null.

set -e
cd "$(dirname "$0")/../.."
SISP=./src/sisp
DATA="${SBTC_DATA:-examples/sbtc/chain.dat}"

CORE="examples/sbtc/hash.lsp examples/sbtc/ec.lsp examples/sbtc/ecdsa.lsp \
examples/sbtc/serialize.lsp examples/sbtc/tx.lsp examples/sbtc/utxo.lsp \
examples/sbtc/block.lsp examples/sbtc/pow.lsp examples/sbtc/chain.lsp \
examples/sbtc/mempool.lsp examples/sbtc/node.lsp examples/sbtc/net.lsp \
examples/sbtc/wallet.lsp"
EXTRA="examples/sbtc/persist.lsp examples/sbtc/cli.lsp"

cmd="${1:-demo}"

case "$cmd" in
  demo)  exec $SISP $CORE $EXTRA examples/sbtc/demo.lsp        < /dev/null ;;
  story) exec $SISP $CORE $EXTRA examples/sbtc/wallet-demo.lsp < /dev/null ;;
  tests) exec $SISP $CORE $EXTRA examples/sbtc/tests.lsp       < /dev/null ;;
esac

# ledger commands: build a one-line driver call
PRE=""
NEED_LEDGER="yes"
case "$cmd" in
  new)     CALL="(cli-new \"$2\")"; NEED_LEDGER="no" ;;
  init)    CALL="(cli-init \"$DATA\" \"${2:-satoshi}\")"; NEED_LEDGER="no" ;;
  faucet)  WHO="${2:-satoshi}"
           [ -f "$DATA" ] || PRE="(cli-init \"$DATA\" \"$WHO\")"
           CALL="(cli-faucet \"$DATA\" \"$WHO\")"; NEED_LEDGER="no" ;;
  mine)    MINER="${2:-satoshi}"
           [ -f "$DATA" ] || PRE="(cli-init \"$DATA\" \"$MINER\")"
           CALL="(cli-mine \"$DATA\" \"$MINER\")"; NEED_LEDGER="no" ;;
  send)    CALL="(cli-send \"$DATA\" \"$2\" \"$3\" $4)" ;;
  balance) CALL="(cli-balance \"$DATA\" \"$2\")" ;;
  status)  CALL="(cli-status \"$DATA\")" ;;
  log)     CALL="(cli-log \"$DATA\")" ;;
  *)       echo "usage: $0 {demo|story|tests|new|init|faucet|mine|send|balance|status|log} ..."; exit 1 ;;
esac
if [ "$NEED_LEDGER" = "yes" ] && [ ! -f "$DATA" ]; then
  echo "no ledger yet; run: $0 init <miner>   (or: $0 faucet <who>)"; exit 1
fi

TMP="$(mktemp /tmp/sbtc-XXXXXX.lsp)"
trap 'rm -f "$TMP"' EXIT
printf '%s\n%s\n' "$PRE" "$CALL" > "$TMP"
$SISP $CORE $EXTRA "$TMP" < /dev/null

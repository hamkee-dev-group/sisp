# sBTC — a real proof-of-work cryptocurrency in pure SISP

sBTC is a working Bitcoin-style cryptocurrency implemented entirely in SISP,
in the same spirit as `examples/machine/` (a whole OS + TCP/IP stack running
inside the interpreter): **internally real, not host-real.** Independent nodes
mine a genuine hash, gossip blocks over a peer topology, verify real signatures,
and converge on the most-work chain — with forks, reorgs, a mempool, fees, and
halving.

## What is genuinely real

- **SHA-256** — the actual FIPS 180-4 algorithm (32-bit words fit natively; the
  bitwise ops are built from arithmetic + byte lookup tables). Verified bit-exact
  against the official vectors:
  `SHA256("")=e3b0c442…7852b855`, `SHA256("abc")=ba7816bf…f20015ad`.
- **ECDSA** — real key generation, signing, and verification over an elliptic
  curve `y² = x³ + 7` (secp256k1's equation). Deterministic RFC-6979-style nonce.
- **UTXO model** — inputs/outputs, ownership by address, real double-spend and
  theft prevention.
- **Proof of work** — miners grind a nonce until the block hash meets a
  difficulty target; tampering with any field invalidates the block.
- **Merkle trees, block subsidy + halving, fees, mempool.**
- **Nakamoto consensus** — the most-cumulative-work chain wins; heavier forks
  trigger a real reorg and the UTXO set follows.
- **P2P gossip** — flood relay over a peer graph; a block mined at one node
  propagates hop by hop until every node agrees.

## What is deliberately shrunk (honest caveats)

SISP integers are 64-bit with no bignum, and there are no host sockets, so:

- **Key/field sizes are tiny** (`p = 100003`, group order `n = 99667`, keyspace
  ≈ 2¹⁶). The ECDSA *algorithm* is real; the *security* is a toy — keys are
  breakable by brute force. This is the intended "real algorithm, small numbers"
  tradeoff (the same one `examples/problems/crypto/` makes).
- **The network is in-process.** "Multiple nodes" are multiple node objects in
  one interpreter; gossip is modeled as relay between them. It cannot talk to the
  real Bitcoin network.
- **Addresses** use `SHA-256d(pubkey)` truncated (no native RIPEMD160).
- **Timestamps** are logical, and there is no coinbase maturity rule.
- **Difficulty is low** so that emulated-SHA mining finishes in seconds.

None of the shrinking makes anything fake: the miner really does the work, the
signatures really prevent forgery, and consensus really emerges from work.

## Running

```sh
examples/sbtc/run.sh          # narrated end-to-end demo
examples/sbtc/run.sh tests    # self-test suite (PASS/FAIL)
```

## Persistent ledger (a real chain that grows on disk)

Unlike the fixed demo, these commands persist the chain to disk
(`$SBTC_DATA`, default `examples/sbtc/chain.dat`). Each command loads the
saved state, does **one** action, and saves — so the blockchain genuinely
accumulates across separate runs and every action is recorded. Identities are
just names (the key is derived deterministically from the name).

```sh
examples/sbtc/run.sh new    alice         # show a wallet (name -> address)
examples/sbtc/run.sh faucet alice         # deposit fictional coins to alice
examples/sbtc/run.sh mine   alice         # alice mines a block (earns reward)
examples/sbtc/run.sh send   alice bob 30  # queue a signed payment (fee 1)
examples/sbtc/run.sh mine   carol         # carol mines -> confirms the payment
examples/sbtc/run.sh balance alice        # show a balance
examples/sbtc/run.sh status               # height, tip, mempool
examples/sbtc/run.sh log                  # recorded block history
```

Each command prints a self-describing result, e.g.
`(mined (height 2) (miner alice) (txs 1) (balance 150))`.
For a scripted end-to-end story of the whole flow:

```sh
examples/sbtc/run.sh story                # wallet -> deposit -> mine -> send
```

The saved state is validated by replaying every block through `chain-add` on
load, so a tampered ledger fails to load rather than being trusted. The on-disk
format is a quote-free S-expression (strings are stored as `(q c0 c1 …)` byte
codes, since SISP string literals can't contain a `"`).

Or directly (note the `< /dev/null` — SISP opens a REPL after its file args):

```sh
./src/sisp examples/sbtc/hash.lsp examples/sbtc/ec.lsp examples/sbtc/ecdsa.lsp \
  examples/sbtc/serialize.lsp examples/sbtc/tx.lsp examples/sbtc/utxo.lsp \
  examples/sbtc/block.lsp examples/sbtc/pow.lsp examples/sbtc/chain.lsp \
  examples/sbtc/mempool.lsp examples/sbtc/node.lsp examples/sbtc/net.lsp \
  examples/sbtc/wallet.lsp examples/sbtc/demo.lsp < /dev/null
```

## Files (dependency order)

| file | role |
|------|------|
| `hash.lsp` | SHA-256 / SHA-256d, hex, byte helpers |
| `ec.lsp` | curve params + point arithmetic |
| `ecdsa.lsp` | keys, sign, verify |
| `serialize.lsp` | canonical byte serialization |
| `tx.lsp` | transactions, txid, sighash, addresses |
| `utxo.lsp` | UTXO set, validation, double-spend prevention |
| `block.lsp` | Merkle tree, block header, block hash |
| `pow.lsp` | target/difficulty, mining, subsidy, halving |
| `chain.lsp` | block store, work, best tip, reorg |
| `mempool.lsp` | pending transactions |
| `node.lsp` | a full node |
| `net.lsp` | P2P gossip + orchestration |
| `wallet.lsp` | coin selection, change, payments |
| `persist.lsp` | serialize/save/load the ledger to disk |
| `cli.lsp` | command-line ledger operations |
| `tests.lsp` | self-tests |
| `demo.lsp` | narrated end-to-end scenario |
| `wallet-demo.lsp` | scripted wallet -> deposit -> mine -> send story |
| `run.sh` | runner + persistent-ledger CLI |

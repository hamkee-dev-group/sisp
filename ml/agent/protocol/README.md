# Agent policy data

`train/` contains 64 two-turn policy episodes and 128 next-action targets in
the exact prompt format built by `agent.lsp`. These files are training data,
not ordinary SISP programs. The corpus builder must keep every file in the
training split and must not apply fill-in-the-middle reordering to them.

The 12 episodes under `../eval/` are held out permanently. They measure the
same five-action policy and recovery behavior with unseen goals and constants;
they must never enter a training, validation, or test corpus shard.

Validate field lengths, vocabulary, action counts, data separation, and every
trajectory's execution with:

```sh
ml/agent/run-protocol-tests.sh
```

Use `--static` only when the interpreter has not been built. The complete gate
loads every trajectory through SISP and checks its tool kind, observation, final
answer action, and persistent-memory transition.

# Native SISP chatbot

This directory is the human-facing layer over the transformer and research
agent.  The chatbot itself is pure SISP.  It performs transformer generation,
model selection, persistent memory, tool-action parsing, and agent execution in
the interpreter.

Large checkpoints and machine-local paths are not committed. Configure
`ml/chatbot/local/chatbot.lsp` with your checkpoint paths. This file should load the published SISP implementation and the selected native
checkpoint or checkpoints.  Start it from the repository root:

```sh
src/sisp ml/chatbot/local/chatbot.lsp
```

The bootstrap loads model code, `vocab-code.txt`, and checkpoint
parts. The corpus is a training input and is not needed by the chatbot.

Launch the native REPL with a configured local bootstrap:

```sh
ml/chatbot/run.sh
```

At the SISP prompt:

```lisp
(chatbot-code "(define (square x) (* x")
(chatbot-ask "What does car return?")
(chatbot-help "mat*")
(chatbot-algorithm "euclid")
(chatbot-agent "Remember that project is sisp, then answer the project name")
(chatbot-status)
```

If the bootstrap contains several checkpoints, switch without restarting:

```lisp
(chatbot-use-model "knowledge")
```

The local bootstrap creates persistent `memory.txt`, `transcript.txt`, and
`action.lsp` beside itself.  Agent output is experimental SISP code and is
evaluated intentionally; use it as a research system, not a security boundary.

The same learning system is published in `ml/llm`: pretraining uses
`matrix-corpus.lsp`, SFT/DPO/REINFORCE/distillation are in `posttrain.lsp` and
`posttrain-session.lsp`, portable checkpoints are in `matrix-checkpoint.lsp`,
and the existing shell runners only schedule top-level batches and files.

Run the small control-layer test with:

```sh
ml/chatbot/run-tests.sh
```

# Pure-SISP model observer

`observer.lsp` is a read-only microscope for the native matrix transformer. It
does all analysis in SISP and prints plain text intended for a human. The shell
runner only selects files and configuration, as it already does for training.
The observer calls forward inference but never backward, AdamW, parameter
mutation, generated-code evaluation, or an external viewer.

## Inspect a checkpoint

Use exactly the profile, corpus, validation corpus, and vocabulary that created
the checkpoint. Strict checkpoint loading checks their metadata and checksums.

```sh
SISP_LLM_PROFILE=code \
SISP_LLM_CORPUS=ml/llm/data/local/sisp-v1/train.txt \
SISP_LLM_VALID_CORPUS=ml/llm/data/local/sisp-v1/valid.txt \
SISP_LLM_RESUME=/srv/sisp/checkpoints/code.best \
SISP_LLM_STEPS=0 SISP_LLM_TOKENS=64 \
SISP_LLM_OBSERVER=1 \
  ml/llm/run-matrix-corpus.sh
```

`SISP_LLM_STEPS=0` is essential when the intent is observation only. Generation
still runs and is useful behavior evidence; `SISP_LLM_TOKENS` controls its
length. The observer's integrity line independently confirms that all parameter
weights have the same norm before and after its forward passes.

For a prompt of your own, put ordinary SISP text in a file and add:

```sh
SISP_LLM_PROMPT_FILE=/path/to/prompt.txt
```

That replaces the observer's deep fixed prompt. It prints the visible context,
next-character distribution, attention, and activations. The five short fixed
teacher-forced probes are used when there is no custom prompt so checkpoints
remain directly comparable.

Set `SISP_LLM_OBSERVER_FULL=1` only when tensor-level output is wanted. It prints
the shape, weight RMS, gradient RMS, both Adam moment RMS values, and AdamW decay
status for every parameter object. It can be long on large profiles.

## Reading the text

- `initial-*` and `final-*` come from the normal runner. Validation NLL and bits
  per byte are the most useful held-out quality measures; lower is better.
- `generated` is the model's sampled continuation. It is the most direct human
  behavior check, but one sample is noisy.
- `observer-model` states the actual scale and structural contract: causal
  learned-position, pre-RMSNorm, residual, multi-head SwiGLU transformer with
  tied token/output weights.
- `observer-training` gives checkpoint step, epoch, processed characters,
  current scheduled learning rate, data sizes, and sampling mode.
- `observer-parameters` gives global weights, gradients, and Adam moment norms.
  Moment norms of zero are expected before any optimization update.
- `observer-probe` asks for one known next character. Learning is visible when
  its probability rises, rank approaches one, and NLL falls. Entropy measures
  uncertainty; lower is more concentrated, though not necessarily more correct.
- `observer-embedding` lists characters whose learned embedding directions are
  closest by cosine similarity. Random models have arbitrary neighbors;
  persistent syntactic groupings after training are more interesting.
- `observer-attention` is reported separately for every actual layer and head at
  the last input position. It gives the attended prompt positions, characters,
  probabilities, and entropy. Heads are never averaged or presented as fake
  explanations.
- `observer-activation` gives RMS scales for input, Q, K, V, SwiGLU up/gate, and
  hidden values. `dominant-hidden` lists the largest-magnitude hidden features at
  the final position. Exploding, vanishing, or permanently dead scales are
  warning signs.
- `observer-contract` checks parameter naming/count, finite numeric state, and
  model/config dimensions. `observer-integrity` and `observer-finished` confirm
  that the observer completed read-only.

Run the same command at successive checkpoints and compare held-out NLL,
generated samples, fixed-probe probability/rank/NLL, attention routing, and
activation health. Feature numbers and attention-head numbers are meaningful
only within the same architecture and training lineage.

## Honest limitation

These reports expose numeric computation, not semantic thoughts. Attention says
where a head routed information; a hidden feature says which coordinate became
large. Neither is a faithful natural-language reason for a prediction, and this
small model has no private written chain of thought to recover. The observer is
for learning curves, failure diagnosis, and concrete inspection—not for claiming
that internal activations are explanations.

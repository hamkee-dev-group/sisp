# SISP code model architecture decision

This is the frozen design record for the first approximately 8 MB SISP-code run.
It separates what is implemented and tested from ideas that remain experiments.

## Decision

Use `code` as the primary model and `code-plus` as a one-epoch challenger.

| Profile | Parameters | Context | Width | Heads | Layers | FF hidden | Microbatch |
|---|---:|---:|---:|---:|---:|---:|---:|
| `code` | 479,552 | 256 | 96 | 4 | 4 | 256 | 4 |
| `code-long` (A/B experiment) | 504,128 | 512 | 96 | 4 | 4 | 256 | 2 |
| `code-plus` | 901,856 | 256 | 128 | 4 | 4 | 384 | 2 |

All three are decoder-only, character-level causal transformers with learned absolute
positions, pre-RMSNorm, multi-head attention, SwiGLU feed-forward blocks, residual
connections, tied input/output embeddings, and an untied output bias. Training is
full-context numerically stable cross-entropy with explicit backpropagation,
global gradient clipping, and AdamW.

`code-long` changes only the learned position table and context length.  It is
an experimental challenger for measuring whether twice the usable SISP prefix
is worth the quadratic attention cost; it is not the default long-run profile.

Learning-rate schedules are normalized for the profile microbatch: `code-plus`
processes 512 target characters per optimizer update versus 1,024 for `code`, so
its default warmup/decay are 1,000/80,000 updates rather than 500/40,000. This
keeps equal-character A/B comparisons at the same schedule position.

The approximately 8 MB corpus provides about 17 characters per `code` parameter or
nine per `code-plus` parameter in one pass. This makes `code` the safer data/compute
match and `code-plus` a useful capacity test. The 1.61M `research` and 8.47M `large`
profiles fit in memory but are not recommended for this corpus: fitting is not the
same as learning efficiently.

## Implemented training contract

- The token embedding is shared with the vocabulary projection. This reduces the
  parameter count and lets input and output evidence update one representation.
- Residual-output matrices use depth-scaled initialization. All other learned
  matrices retain the profile scale.
- AdamW decays learned weight matrices and embeddings, but not biases or RMSNorm
  gains. The default second-moment coefficient is 0.95 for this small code model.
- Training windows use a deterministic hashed position schedule derived from the
  saved sequence count. Stop/resume is therefore exact without serializing an RNG.
  Sequential sampling remains available as a diagnostic. Prepared-corpus spans
  prevent a context from crossing between unrelated source units.
- Validation windows are spread from the beginning through the end of the held-out
  corpus instead of measuring only its first few contexts and obey the same unit
  boundaries as training. The unattended `code`/`code-plus` runner selects from
  256 fixed windows (65,536 target characters); resumed chunks skip the redundant
  initial evaluation and use longer chunks to amortize this stronger measurement.
- Cross-entropy uses the native fused stable softmax in its common path and exact
  log-sum-exp if a target probability underflows. It never clamps the probability
  or caps the reported loss.
- Every update and every checkpoint publication rejects non-finite losses,
  parameters, gradients, or optimizer moments.
- Checkpoint format 3 binds architecture, corpus/vocabulary checksums, sampling,
  optimizer schedule, clipping, and every AdamW state matrix. Duplicate or missing
  components are rejected. Publication is staged and retains the last checkpoint.
- A SHA-256 manifest binds every checkpoint part, state file, and metrics file;
  unattended training can recover a verified previous publication without ever
  silently replacing damaged state with random weights.
- A continuous trainer uses short, restartable chunks, an ownership lock, a
  best-validation checkpoint, optional patience, and elapsed-time limits.
- Short prompts are evaluated at their actual length. Generation begins sliding
  only after the model context is full. Full-context generation reuses the cached
  causal mask and vocabulary projection is restricted to the final hidden row.
- A frozen functional suite measures completion likelihood/top-k accuracy and then
  executes greedy candidates against hidden SISP checks.

## M4 measurements

Measurements used the optimized native build on a 16 GB M4 MacBook Air and the
current explicit-backprop implementation. They are planning data, not quality
results for the final corpus.

| Profile/run | Target chars | Time | Peak RSS | Validation NLL change |
|---|---:|---:|---:|---:|
| `code`, 50 updates, microbatch 4 | 51,200 | 66.5 s | 289 MiB | 4.561 to 4.112 |
| `code-plus`, 30 updates, microbatch 2 | 15,360 | 44.0 s | 368 MB | 4.505 to 4.320 |

The short runs were inside warmup and used an earlier warmup length, so they do
not select a winner. They imply roughly three hours per 8 MB pass for `code` and
six to seven hours for `code-plus` before VM and sustained-thermal effects. Run a
short benchmark on the actual VM, then compare both profiles after the same number
of processed characters and at least one complete pass.

## Why this resembles a real LLM

The project now has the important semantic pieces of contemporary decoder-only
training: causal attention, pre-normalization, a gated feed-forward network,
weight tying, full-sequence stable cross-entropy, AdamW parameter groups,
warmup and cosine decay, gradient accumulation/clipping, held-out evaluation,
temperature/top-k/top-p stochastic sampling, exact checkpoints, and repeatable
continuation. The implementation is small and slow, but the learning problem is
real rather than a scripted response.

The design follows primary literature where it transfers cleanly to this scale:

- [RMSNorm](https://arxiv.org/abs/1910.07467) and
  [pre-layer normalization](https://arxiv.org/abs/2002.04745);
- [GLU variants including SwiGLU](https://arxiv.org/abs/2002.05202);
- [weight tying](https://arxiv.org/abs/1608.05859);
- [decoupled AdamW](https://arxiv.org/abs/1711.05101);
- [TinyStories](https://arxiv.org/abs/2305.07759) as evidence that narrow,
  deliberately constructed data can make very small models coherent;
- [Chinchilla scaling](https://arxiv.org/abs/2203.15556) only as a warning against
  using far more parameters than the available data/compute can train. Its numeric
  optimum was measured at vastly larger scales and is not copied as a law here.

## Deliberately deferred experiments

These are plausible, but adding all of them before the baseline would make failures
hard to diagnose:

- RoPE or ALiBi positional encoding;
- dedicated infilling generation (training-only FIM augmentation is available as
  an optional deterministic A/B experiment);
- a learned byte/subword tokenizer;
- dropout or other regularization;
- KV-cached generation;
- beam search, syntax-constrained decoding, or multiple ranked completions;
- mixed precision or binary checkpoints.

The next architectural change should be justified by an A/B result on the frozen
validation and completion suites. The likely first candidates after the baseline
are fill-in-the-middle for editing and a tokenizer only if character modeling is
shown to waste too much context.

## First-run protocol

1. Freeze `train.txt`, `valid.txt`, `test.txt`, vocabulary, and their checksums.
2. Smoke-test 25–50 updates of both profiles on the actual VM.
3. Train both through the same number of corpus characters and at least one pass.
4. Select using held-out bits per byte, syntax/completion tests, throughput, and
   memory—not training loss alone.
5. Continue only the selected checkpoint. Do not resize it mid-run.
6. Stop when validation ceases to improve for the configured number of chunks;
   retain both the last and the best checkpoint.
7. Evaluate `test.txt` once for the final reported model.
   Use `run-matrix-test.sh`, which is inference-only and cannot save.

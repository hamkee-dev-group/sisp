# A runnable miniature LLM stack in SISP

This directory contains two integrated neural-network stacks written in SISP. The
small reference stack uses scalar reverse-mode autodiff and list tensors. The
larger stack uses SISP's native contiguous f64 matrices and explicit matrix-level
backpropagation. Both include tokenization, causal self-attention, optimization,
training, and generation.

It remains deliberately small. The fast regression model has 130 parameters, a
context length of three, one attention head, and one transformer block. A separate
configurable corpus runner scales the same implementation to several thousand
parameters and trains from a text file. The native path scales to more than eight
million parameters with multiple heads and layers while keeping every backward
formula visible in Lisp.

## Run

From the repository root:

```sh
src/sisp ml/llm/autograd.lsp       # 19 autodiff invariants
src/sisp ml/llm/attention.lsp      # stable forward-only attention
ml/llm/run-xor.sh                  # shared-library MLP training
ml/llm/run-charlm.sh               # neural bigram baseline
ml/llm/run-transformer.sh          # train and sample the causal transformer
ml/llm/run-corpus-transformer.sh   # selectable real-text training profiles
ml/llm/run-matrix-tests.sh         # finite-difference native backward checks
ml/llm/run-matrix-corpus.sh        # native multi-head/multi-layer transformer
ml/llm/run-matrix-checkpoint-tests.sh # exact stop/reload/resume check
ml/llm/run-matrix-forever.sh       # checkpointed unattended training loop
ml/llm/run-reinforce.sh            # autodifferentiated REINFORCE bandit
ml/llm/run-native-posttrain.sh      # config-driven native SFT/DPO/RL batch
ml/llm/run-tests.sh                # fast core regression checks
ml/llm/run-all.sh                  # every LLM check, including training
```

### Test the trained code model interactively

The terminal frontend uses the native SISP model and checkpoint; it contains no
Python inference code. From the repository root run:

```sh
ml/llm/run-interactive.sh
```

Enter a SISP code prefix directly, for example `(define (square x) (* x`.
Commands `:help NAME`, `:algorithm NAME`, `:count N`, `:sample on`, and `:quit`
are also available. A single prompt can be passed without entering the terminal:

```sh
ml/llm/run-interactive.sh '(define (square x) (* x'
```

To test the separately trained source-knowledge model, run:

```sh
SISP_LLM_INTERACTIVE_MODEL=knowledge ml/llm/run-interactive.sh
```

Use `:ask QUESTION`, for example `:ask Which C handler implements the SISP name
car?`. The knowledge checkpoint is an experimental research result: it learned
the documentation format but its factual answers are not reliable.

The default native checkpoint is the ignored local directory
`ml/llm/checkpoints/code-best`. Set
`SISP_LLM_INTERACTIVE_CHECKPOINT=/path/to/checkpoint` to test another compatible
checkpoint. Knowledge mode defaults to the ignored local directory
`ml/llm/checkpoints/knowledge-best`. Interactive inference reads only the
selected checkpoint and `vocab-code.txt`; it does not load or require a training
or validation corpus. The shell file only performs mechanical top-level
scheduling; model setup, tokenization, transformer inference, sampling, and
decoding are SISP.

Use the runner scripts for training examples. They pass setup, individual training
steps, and reporting as separate files to one persistent SISP process. Each step is
therefore a top-level form, allowing SISP's between-form garbage collector to
reclaim its temporary computation graph. A recursive `(train 500 ...)` call keeps
all 500 graphs alive until it returns and uses much more memory.

Never print or echo a raw autodiff Value. Its backward closure is cyclic. Print
`(vdata value)`, `(vgrad value)`, or `(value-summary value)`. The runners redirect
stdin from `/dev/null`, preventing REPL echo.

## What is integrated now

The causal transformer follows one continuous differentiable path:

```text
source string
  -> printable-ASCII character token IDs
  -> token embeddings + learned position embeddings
  -> learned pre-RMSNorm + causal multi-head attention + residual
  -> learned pre-RMSNorm + SwiGLU feed-forward + residual
  -> learned final RMSNorm + vocabulary logits at every position
  -> mean full-sequence cross-entropy
  -> explicit backward + gradient accumulation and clipping
  -> warmup/cosine-decay AdamW update
  -> temperature/top-k/top-p autoregressive sampling and decoding
```

The fast training corpus repeats `abcabd`. The contexts `cab` and `dab` both end in
`ab`, but their correct continuations differ:

- `cab -> d`
- `dab -> c`

The bigram baseline cannot distinguish those cases and assigns similar probability
to `c` and `d` after `b`. The transformer uses earlier context and learns both
predictions. With the fixed seed used here, a representative run reduces mean NLL
from about 1.43 to 0.28 and generates text resembling the training grammar. On the
development VM it takes roughly 17–19 seconds and peaks near 75 MB sampled RSS.
On a 10-core M4 MacBook Air it takes about 5 seconds and peaks near 53 MB.
For comparison, a recursive nine-parameter bigram training loop retains hundreds
of training graphs and peaks near 880 MB; top-level-step scheduling is therefore
doing materially useful work.

## Shared libraries

| File | Responsibility |
|---|---|
| `engine.lsp` | Tagged scalar Values, differentiable operations, graph traversal, backward pass |
| `math.lsp` | Stable softmax, logsumexp cross-entropy, categorical sampling, argmax |
| `tensor.lsp` | Shape-checked list vectors/matrices and plain-number matrix helpers |
| `optim.lsp` | Gradient clearing, global-norm clipping, SGD, parameter counts |
| `data.lsp` | Character encoding/decoding and autoregressive windows |
| `nn.lsp` | Linear layers, embeddings, RMS normalization, causal attention, transformer block |
| `model.lsp` | Transformer assembly, loss, generation, and one-step training |
| `metrics.lsp` | Floating-point assertions and test summaries |
| `matrix-engine.lsp` | Matrix parameters, gradients, AdamW, clipping, RMSNorm and softmax backward |
| `matrix-nn.lsp` | Explicitly differentiated linear, attention, and transformer-block layers |
| `matrix-model.lsp` | Multi-head/multi-layer causal transformer, loss, backward, and sampling |
| `matrix-checkpoint.lsp` | Exact text checkpoints for parameters, AdamW state, and training position |
| `posttrain.lsp` | Completion-only SFT, DPO, REINFORCE, and distillation objectives |
| `posttrain-session.lsp` | Top-level native scoring, gradient, optimizer, and checkpoint state machine |

## Models and demonstrations

| File or runner | Purpose |
|---|---|
| `autograd.lsp` | Analytic checks for every scalar derivative, shared graphs, repeat behavior, snapshots |
| `attention.lsp` | Stable forward attention and probability/shape invariants |
| `run-xor.sh` | A 2-4-1 MLP using the same layers, Values, optimizer, and tests |
| `run-charlm.sh` | A 16-parameter neural bigram baseline on the transformer corpus |
| `run-transformer.sh` | A 130-parameter trainable causal character transformer |
| `run-corpus-transformer.sh` | File-backed text training with three M4-sized profiles |
| `run-matrix-tests.sh` | End-to-end finite-difference validation of native matrix backprop |
| `run-matrix-corpus.sh` | Native matrix transformer from 6,176 to 8,468,832 parameters |
| `run-reinforce.sh` | REINFORCE loss `-advantage * log pi(action)` differentiated by `engine.lsp` |
| `native-posttrain-config.lsp` | All posttraining mode, path, sampling, reward, and optimizer settings |
| `native-posttrain-main.lsp` | Complete SISP SFT, DPO, or REINFORCE program |
| `run-native-posttrain.sh` | Mechanical checkpoint loading and failure-isolation batch boundary |

The scalar bandit remains an educational example. Real transformer post-training
uses `posttrain.lsp` and the selected 901,856-parameter knowledge checkpoint. See
[POSTTRAINING.md](POSTTRAINING.md) for configuration, objectives, and measured
results.

Files named `*-train.lsp` intentionally contain repeated top-level calls. They are
mechanical training schedules, not library code. Files named `*-report.lsp` compute
final metrics and assertions after the same interpreter process has trained the
model.

## Native matrix transformer

This is the main scaling path. It does not use the scalar autodiff engine.
Forward and backward are written explicitly with native matrix kernels, so a
matmul is one Lisp operation over contiguous doubles rather than a graph of scalar
closures.

```sh
# Fast native training.
ml/llm/run-matrix-corpus.sh

# 273,312 parameters, context 32, four heads and four layers.
SISP_LLM_PROFILE=practical ml/llm/run-matrix-corpus.sh

# Primary profile for the approximately 8 MB SISP-code corpus.
SISP_LLM_PROFILE=code SISP_LLM_STEPS=50 SISP_LLM_TOKENS=8 \
  ml/llm/run-matrix-corpus.sh

# Larger one-epoch challenger, compared by equal processed characters.
SISP_LLM_PROFILE=code-plus SISP_LLM_STEPS=25 SISP_LLM_TOKENS=8 \
  ml/llm/run-matrix-corpus.sh

# 8.47M parameters; a tested fit/compute point, not a corpus recommendation.
SISP_LLM_PROFILE=large SISP_LLM_STEPS=1 SISP_LLM_TOKENS=1 \
  ml/llm/run-matrix-corpus.sh
```

| Profile | Parameters | Context | Width | Heads | Layers | Hidden | Default microbatch | Role |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| `small` | 6,176 | 16 | 16 | 2 | 1 | 64 | 2 | regression/smoke |
| `practical` | 273,312 | 32 | 64 | 4 | 4 | 256 | 4 | fast compatibility |
| `code` | 479,552 | 256 | 96 | 4 | 4 | 256 | 4 | primary 8 MB run |
| `code-plus` | 901,856 | 256 | 128 | 4 | 4 | 384 | 2 | one-epoch challenger |
| `research` | 1,610,208 | 128 | 128 | 8 | 6 | 512 | 4 | scaling experiment |
| `large` | 8,468,832 | 128 | 256 | 8 | 8 | 1,024 | 2 | fit/compute experiment |

A current optimized M4 benchmark of `code` trained 51,200 characters in 66.5
seconds and peaked near 289 MiB; validation NLL moved from 4.561 to 4.112. A
`code-plus` short run trained 15,360 characters in 44 seconds and peaked near 368
MB; validation NLL moved from 4.505 to 4.320. These are warmup-stage planning
measurements on a small corpus, not a quality comparison. They imply approximately
three versus six-to-seven hours per 8 MB pass on this Mac. Benchmark the VM and
select after equal processed characters and at least one full pass.

The model uses tied token/output embeddings, depth-scaled residual initialization,
pre-RMSNorm, SwiGLU, and explicit full-sequence backpropagation. AdamW decays
matrices/embeddings but excludes biases and normalization gains. Deterministic
hashed window sampling is exactly resumable, and held-out evaluation is spread
across the complete validation corpus. See [ARCHITECTURE.md](ARCHITECTURE.md) for
the design evidence, measured tradeoffs, and deferred experiments.

`run-matrix-tests.sh` checks explicit derivatives against central finite
differences at representative locations in token and position embeddings, Q/K/V/O
attention matrices, SwiGLU weights, learned normalization gains, feed-forward
weights/biases, and the tied language projection. It also tests causal isolation,
full-sequence stable cross-entropy with exact log-sum-exp underflow fallback,
top-k/top-p selection, uncapped extreme loss, gradient clipping, and AdamW decay
groups.

The current native `mat*` is a portable triple loop. A 512-square multiplication
takes about 380 ms (roughly 0.71 GFLOP/s) on the measured M4. BLAS/Accelerate is
therefore the next performance target. Binary checkpoints would be a useful
optimization, but are not required for unattended training: the existing file
and matrix APIs provide exact reader-loadable checkpoints.

### Pure-SISP model microscope

The optional observer is itself SISP code. It uses the transformer's real forward
cache and native matrix operations; there is no Python analysis program, chart,
browser, or model-specific interpreter support.

```sh
# Inspect a saved model without training it.
SISP_LLM_PROFILE=code SISP_LLM_STEPS=0 SISP_LLM_TOKENS=64 \
SISP_LLM_RESUME=/srv/sisp/checkpoints/code.best \
SISP_LLM_OBSERVER=1 \
  ml/llm/run-matrix-corpus.sh

# Include one line for every parameter tensor.
SISP_LLM_PROFILE=code SISP_LLM_STEPS=0 SISP_LLM_TOKENS=1 \
SISP_LLM_RESUME=/srv/sisp/checkpoints/code.best \
SISP_LLM_OBSERVER=1 SISP_LLM_OBSERVER_FULL=1 \
  ml/llm/run-matrix-corpus.sh
```

The normal report supplies training/validation NLL and generated text. The
observer adds architecture and optimizer-state facts, fixed next-character
probes, top probabilities, embedding neighbors, genuine per-head attention,
per-layer activation scales, dominant hidden features, and an explicit check
that observation changed no weights. Its fixed probes can be compared after
every checkpoint to see learning numerically. Attention and hidden activations
are internal signals, not a chain-of-thought explanation. See
[`OBSERVER.md`](OBSERVER.md) for the fields and a custom-prompt command.

## Checkpoints and continuous VM training

`run-matrix-corpus.sh` can save after a training chunk or resume a saved chunk.
The checkpoint includes every parameter, both AdamW moment matrices, optimizer
step, deterministic sampling state, architecture/corpus metadata, and optimizer
settings. Every published directory also carries a SHA-256 integrity manifest;
training resume refuses missing, mixed, truncated, modified, or corpus-mismatched
files. Inference validates checkpoint integrity, architecture, vocabulary, and
finite parameters, but deliberately does not require the original corpus.

```sh
# Train 50 updates and publish a checkpoint directory.
SISP_LLM_PROFILE=code SISP_LLM_STEPS=50 \
  SISP_LLM_SAVE=/srv/sisp/checkpoints/code \
  ml/llm/run-matrix-corpus.sh

# Restart later, or copy the directory to another machine and continue exactly.
SISP_LLM_PROFILE=code SISP_LLM_STEPS=50 \
  SISP_LLM_RESUME=/srv/sisp/checkpoints/code \
  SISP_LLM_SAVE=/srv/sisp/checkpoints/code \
  ml/llm/run-matrix-corpus.sh

# Run continuously in restartable profile-sized chunks on a VM.
SISP_LLM_PROFILE=code \
SISP_LLM_CHECKPOINT=/srv/sisp/checkpoints/code \
SISP_LLM_LOG=/srv/sisp/code.log \
  ml/llm/run-matrix-forever.sh

# Stop after the current safe chunk once roughly 24 hours have elapsed.
SISP_LLM_PROFILE=code SISP_LLM_MAX_HOURS=24 \
SISP_LLM_CHECKPOINT=/srv/sisp/checkpoints/code \
  ml/llm/run-matrix-forever.sh

# Generate from your own prompt without changing a saved model.
SISP_LLM_PROFILE=code SISP_LLM_STEPS=0 SISP_LLM_TOKENS=128 \
SISP_LLM_RESUME=/srv/sisp/checkpoints/code.best \
SISP_LLM_PROMPT_FILE=ml/llm/prompt-example.txt \
  ml/llm/run-matrix-corpus.sh
```

The continuous runner starts from random weights only when no checkpoint exists.
After each chunk it starts a fresh SISP process, resumes the prior state, trains,
stages a new checkpoint, and publishes it only after a complete successful write.
The former checkpoint remains at `<checkpoint>.previous`. Killing a run during
training or checkpoint writing therefore leaves the last published checkpoint
usable. A per-checkpoint lock rejects concurrent continuous trainers; remove a
stale `.lock` directory after a kill that cannot run the cleanup trap. A process
supervisor such as systemd can restart the runner after reboot.
If the current publication fails integrity verification but `.previous` is valid,
the runner quarantines the damaged directory and resumes the verified previous
state. It stops rather than silently starting from random weights when checkpoint
artifacts exist but neither copy verifies.
`SISP_LLM_MAX_HOURS`, `SISP_LLM_MAX_CHUNKS`, and optional validation patience make
fixed-duration runs straightforward. The lowest-validation-loss checkpoint is
copied to `<checkpoint>.best`; set `SISP_LLM_KEEP_BEST=0` to avoid that extra disk
copy. Continuous `code` and `code-plus` runs use 256 distributed validation
windows for checkpoint selection. Resumed chunks omit a redundant initial
evaluation; override the frozen selection size only with
`SISP_LLM_EVAL_SEQUENCES` before the run begins.

Generation defaults to temperature `0.3`, top-k `5`, and top-p `0.95`. Override
them with `SISP_LLM_TEMPERATURE`, `SISP_LLM_TOP_K`, and `SISP_LLM_TOP_P`; set
top-p to `1.0` to retain the complete top-k candidate set.

Inference-only mode requires `SISP_LLM_STEPS=0`. A prompt may be any non-empty
printable-ASCII text file. Short prompts run at their true length with the matching
slice of the learned position table and causal mask; generation begins sliding the
window only after the context is full. Strict resume still requires the same
vocabulary, corpus, and unit-span checksums used to create the checkpoint, so pass
custom corpus paths again when loading such a model.

The sampling path clamps short-history slicing, rejects an empty history, reuses
the cached full-context causal mask, and projects only the final hidden row through
the vocabulary head. It still recomputes transformer states rather than maintaining
a KV cache.

Checkpoint format 3 uses one reader-loadable file for each parameter or AdamW
component. This is slower/larger than binary, but uses only current SISP facilities
and keeps every file far below the 256 MB read limit. It validates the model
contract, corpus/vocabulary checksums, optimizer/sampling configuration, component
count/uniqueness, and finite state before publication. The text size is roughly
proportional to three times the model parameters because weights and both AdamW
moments are saved; measure free disk space before a long run.

`run-matrix-checkpoint-tests.sh` proves exact continuation by comparing a stopped
two-step run plus a resumed third step against an uninterrupted three-step run.
All checkpoint files match byte-for-byte, including weights and optimizer state.
It also verifies rejection of incompatible profiles/configuration and duplicate
logical components, verified recovery from a damaged current publication, and a
read-only final-test evaluation.

## Preparing a real corpus

See [`../training/README.md`](../training/README.md) for unit format, validation,
and corpus assembly. The deterministic preparation tool
normalizes files, filters comments, exact-deduplicates, detects token-shingle near
duplicates, and keeps each semantic generator family in one split. It emits a
manifest, report, and span metadata so model contexts never cross files:

```sh
ml/training/harness/build-corpus.sh \
  ml/llm/data/local/sisp-v1

SISP_LLM_PROFILE=code \
SISP_LLM_CORPUS=ml/llm/data/local/sisp-v1/train.txt \
SISP_LLM_VALID_CORPUS=ml/llm/data/local/sisp-v1/valid.txt \
  ml/llm/run-matrix-corpus.sh
```

The preparation tool is intentionally not a trust or compatibility oracle.
Additional programs need source-permission checks, near-duplicate review,
and independent validation of their expected behavior.
The functional benchmark, its hidden checks, distilled copies of those checks,
and held-out agent-policy tasks are explicitly excluded from corpus construction.

Optional fill-in-the-middle augmentation is deterministic and training-only:

```sh
SISP_LLM_FIM_RATE=0.15 SISP_LLM_FIM_SEED=314159 \
  ml/training/harness/build-corpus.sh \
  ml/llm/data/local/sisp-v1-fim
```

Keep the ordinary corpus as the baseline and enable FIM only as a measured A/B
experiment. To run the frozen executable-completion suite against a checkpoint:

```sh
SISP_LLM_PROFILE=code \
SISP_LLM_CORPUS=ml/llm/data/local/sisp-v1/train.txt \
SISP_LLM_VALID_CORPUS=ml/llm/data/local/sisp-v1/valid.txt \
SISP_LLM_RESUME=/srv/sisp/checkpoints/code.best \
  ml/llm/run-functional-eval.sh
```

It reports teacher-forced NLL/top-1/top-5 and executes every greedy completion
against a hidden SISP check. Any failed executable check makes the command fail.
Quality evaluation requires `SISP_LLM_RESUME`; only `SISP_LLM_ALLOW_RANDOM=1` enables the tiny
random smoke model.

After model selection is completely finished, report the untouched test split
exactly once. The command restores against the original frozen train/validation
contract, substitutes test data only after restoration, performs no updates, and
cannot publish a checkpoint:

```sh
SISP_LLM_PROFILE=code \
SISP_LLM_CORPUS=ml/llm/data/local/sisp-v1/train.txt \
SISP_LLM_VALID_CORPUS=ml/llm/data/local/sisp-v1/valid.txt \
SISP_LLM_TEST_CORPUS=ml/llm/data/local/sisp-v1/test.txt \
SISP_LLM_RESUME=/srv/sisp/checkpoints/code.best \
  ml/llm/run-matrix-test.sh
```

## Scalar corpus model and M4 profiles

The corpus runner reads `corpus.txt` and schedules both training and sampling as
separate top-level forms. Recursive multi-token generation retains every forward
graph just as recursive training retains every backward graph.

```sh
# About 1.5–2 minutes on the measured M4 Air.
ml/llm/run-corpus-transformer.sh

# Sustained-training target: about 20 minutes for the default 1,200 steps.
SISP_LLM_PROFILE=practical ml/llm/run-corpus-transformer.sh

# Override training steps or generated tokens for experiments and smoke tests.
SISP_LLM_PROFILE=research SISP_LLM_STEPS=10 SISP_LLM_TOKENS=8 \
  ml/llm/run-corpus-transformer.sh
```

| Profile | Context | Width | Hidden | Parameters | Intended use |
|---|---:|---:|---:|---:|---|
| `small` | 8 | 8 | 16 | 887 | Quick corpus training and iteration |
| `practical` | 16 | 16 | 32 | 2,911 | Best sustained M4 Air experiment |
| `research` | 24 | 24 | 48 | 6,087 | Slow scaling and quality experiments |

The fixed corpus is deliberately small and repetitive so learning can be observed
without misrepresenting the result as a general language model. Replace
`corpus.txt` and its explicit vocabulary together when experimenting with other
text.

## Measured scalar scaling on a 16 GB M4 MacBook Air

These are isolated single-step measurements with vocabulary and feed-forward width
equal to twice the model width. Peak RSS includes temporary evaluator and autodiff
allocations in that top-level form.

| Context / width | Parameters | Graph nodes | Seconds / step | Peak RSS |
|---:|---:|---:|---:|---:|
| 8 | 904 | 4,652 | 0.17 | 77 MB |
| 16 | 3,472 | 26,188 | 1.03 | 430 MB |
| 24 | 7,704 | 76,908 | 3.16 | 1.32 GB |
| 32 | 14,624 | 169,100 | 7.65 | 2.96 GB |
| 40 | 21,160 | 315,052 | 15.83 | 4.86 GB |
| 48 | 30,384 | 527,052 | 28.98 | 6.40 GB |

The 30,384-parameter point is a hardware-feasible one-step ceiling, not a useful
training target: 1,000 unthrottled steps would already take roughly eight hours.
Context/width 16 is the practical sustained target. Width 24 is reasonable for a
short research run. Larger runs spend far more memory on transient cons cells and
scalar closures than on parameters.

Dot products and attention reductions use balanced addition trees. This preserves
their derivatives while reducing graph depth and avoiding evaluator-stack failure.
Graph and parameter diagnostics use traversal counters or folds rather than
recursive `length`, so measurement does not impose a lower ceiling than training.

## Correctness contracts

- `backward` clears gradients on every node reachable from its root before running
  one reverse pass. Optimizers also clear all model parameters, including parameters
  absent from a particular example.
- Backward closures capture forward-pass numeric operands. Mutating a leaf after
  forward evaluation does not change the derivative of that completed operation.
- Traversals use generation marks rather than persistent Boolean `seen` flags.
- The global topological list is cleared after traversal so it does not retain a
  completed graph across the next top-level collection.
- Softmax always subtracts its maximum. Scalar cross-entropy uses log-sum-exp
  directly; matrix cross-entropy uses fused stable softmax plus exact log-sum-exp
  fallback if the target probability underflows.
- Vector pair operations require equal shapes. Causal attention at position `i`
  receives keys and values only from positions `0..i`.
- Training examples use fixed seeds and assert mathematical or behavioral
  properties rather than merely printing plausible output.

## Testing

`run-tests.sh` is fast and checks:

- all scalar backward rules;
- shared-subgraph accumulation and repeated-backward semantics;
- stable softmax with logits near 1,000;
- fused cross-entropy values and gradients;
- known matrix multiplication;
- tokenizer round trips and training windows;
- a known SGD update;
- the causal no-future-leak invariant;
- interpreter statistics/runtime availability.

`run-all.sh` additionally trains every model and rejects any `(fail ...)` result or
unexpected stderr. The transformer checks loss reduction, parameter count,
generation length, and both contextual continuations. Every suite has a portable
wall-clock limit; override the 900-second default with
`SISP_LLM_TEST_TIMEOUT_SECONDS`.

## Honest limits

- The reference path still uses cons-list tensors and one closure-backed Value per
  scalar operation. It remains useful for small mathematical comparisons.
- The native path is contiguous f64 and manually differentiated, but its matmul is
  not yet connected to BLAS/Accelerate.
- Garbage collection is safe only between top-level forms, hence the runner design.
- Native matrices are rank two, f64 only, and currently use Lisp integer lists for
  gather/scatter indices.
- Checkpoints are portable reader-loadable decimal f64 data rather than compact
  binary files. They are exact but slower to save and load.
- Generation currently recomputes a full context rather than using a KV cache.
- The bundled corpus is intentionally tiny; larger architecture does not turn that
  dataset into a useful general-language model.

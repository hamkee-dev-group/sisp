# Source-grounded SISP knowledge corpus

`sisp-knowledge-v1` is a separate teaching corpus for the explanatory SISP
model. It does not replace or silently enter the validated code seed corpus.

The authority order is intentional:

1. `src/*.c` and `src/*.h` define interpreter behavior.
2. `src/prelude.lsp` defines the embedded standard library and house style.
3. `tests/*.lsp` demonstrate language contracts.
4. executable `examples/**/*.lsp` teach algorithms and complete systems.
5. example documentation supplies the associated CS and mathematics language.

The generator extracts builtin-to-C-handler mappings, implementation areas,
source comments, prelude and example signatures, parameters, direct call
relationships, short complete definitions, demonstrations, and documented
subject facts. It renders several short question/answer styles because the
student must recognize natural variations of the same request. Every record
retains its source path.

Generate the corpus from the repository root:

```sh
ml/training/harness/build-knowledge-corpus.py
```

Output is written to the ignored directory
`ml/llm/data/local/sisp-knowledge-v1/`:

```text
train.txt
train-spans.lsp
valid.txt
valid-spans.lsp
test.txt
test-spans.lsp
manifest.json
```

The default build targets 24 MiB in total, leaving more than 20 MB in the
training split. Every record is printable ASCII plus newlines, exactly
deduplicated, and at most 224 bytes. The remaining 32 characters give sampling
windows room to contain a complete interaction inside the model's 256-character
context. All records derived from one source file
remain in the same split. Interpreter source, prelude, and tests are always in
training; validation and test hold out complete example sources rather than
leaking alternate phrasings of training records.

Each split has a matching SISP matrix span file. The span covers the complete
split so a 256-character training window can contain a short question/answer
record plus its separator and neighboring context.

The manifest records inputs, source assignments, record kinds, sizes, and
SHA-256 checksums. The corpus intentionally distinguishes factual extraction
from invention: source-local relationship records say that definitions occur
together or directly call one another; they do not claim undocumented
semantics. Long definitions are labeled as source excerpts rather than emitted
as apparently complete code.

This corpus teaches short explanations and retrieval grounded in SISP. It does
not make a sub-million-parameter transformer a general reasoner, and validation
on held-out source files must remain separate from memorized reference facts.

## First 901k-parameter experiment

The `code-plus` transformer was trained from scratch on the first frozen build
for two corpus-equivalent passes on MPS. Validation sources were completely
absent from training.

| Passes | Steps | Held-out NLL | Last batch NLL | Run time |
| ---: | ---: | ---: | ---: | ---: |
| 1.0 | 1,398 | 1.083780 | 0.363537 | 467.4 s |
| 1.5 | 2,097 | 1.032780 | 0.303553 | 375.6 s |
| 2.0 | 2,796 | 1.018822 | 0.306112 | 387.3 s |

The optimizer reached its minimum learning rate and training stopped. The model
learned documentation syntax and locally plausible SISP terminology, but exact
fact probes remained unreliable. It answered the C handler for `car` as
`F_cap`, the visible return of `F_car` as `nil`, and the signature of `reverse`
as `(reverse repeated result)`. Low character NLL is therefore dominated by
predictable record structure and language; it must not be interpreted as
faithful source recall. This is an observed capacity/result limitation, not a
reason to weaken the source-grounded corpus or leak validation sources.

The selected checkpoint has 901,856 parameters and was successfully exported
and loaded by unchanged native SISP as 100 parameter objects. The native text
checkpoint is 54,784,962 bytes.

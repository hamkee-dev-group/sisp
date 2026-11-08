# Corpus preparation

This directory contains tools for preparing executable SISP training units and
source-derived documentation records. Model code and training runners live in
[`../llm/`](../llm/README.md).

## Executable units

Put local code units in `seed/*.lsp` or `gen/*.lsp`. Neither directory is required
for a checkout to build. The generated `gen/` directory is ignored by Git.
Each unit should be self-contained, with a short intent comment, definitions,
and example calls annotated with their expected results:

```lisp
; Square a number.
(define (square x) (* x x))
(square 7) ; => 49
```

Check an individual file against the built interpreter:

```sh
sh ml/training/harness/check-oracles.sh path/to/unit.lsp
```

Build the local corpus:

```sh
make
ml/training/harness/build-corpus.sh
```

The builder validates code-unit oracles and the agent protocol, then runs
`ml/llm/prepare-corpus.py`. It normalizes text, checks the frozen vocabulary,
removes exact duplicates, and groups semantic families and detected near
duplicates into the same split. Agent training trajectories are included;
held-out functional evaluation tasks and known distilled copies are excluded.
With no local code units, the build contains only the bundled agent trajectories.

Outputs go to `ml/llm/data/local/sisp-v1/`: train, validation, and test text;
span files; a manifest with hashes and source metadata; and a preparation report.
Source classes follow directory paths. Generator identity is left unspecified;
filenames are not treated as evidence of authorship.
Review source permissions, oracle quality, and near-duplicate reports before
using additional data. Examples and tests are references for writing compact
units, rather than direct inputs to this builder.

## Documentation records

`harness/build-knowledge-corpus.py` derives short records from interpreter source,
the prelude, tests, examples, and documentation. See
[KNOWLEDGE-CORPUS.md](KNOWLEDGE-CORPUS.md) for its output and split contract.

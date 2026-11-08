# SISP

SISP is a Lisp interpreter written in C, with exact rational arithmetic, floating
point numbers, sets and set comprehensions, and native matrix operations. The
repository includes reusable Lisp modules, algorithms, runnable examples, and
machine learning experiments.

## Build and run

A C11 compiler, POSIX.1-2008 environment, Make, and the C math library are required.

```sh
make
./src/sisp
```

Run a program from the repository root:

```sh
./src/sisp examples/math/combinatorics/pascal-triangle.lsp </dev/null
```

File arguments load in order, followed by standard input. With a terminal, SISP
opens a REPL after loading the files. Use `-i` to force interactive evaluation
and echoed results when piping input.

```lisp
(define (square x) (* x x))
(square 12)                         ; => 144
(/ 1 3)                             ; => 1/3
(union {1 2} {2 3})                  ; => {1 2 3}
(mat* [[1 2][3 4]] [[1][1]])         ; => [[3.0][7.0]]
(doc map)
```

The prelude is embedded during compilation. Library and example paths resolve
relative to the current working directory.

## Checks

```sh
make test          # Interpreter regressions and golden arithmetic corpus
make lint          # cppcheck, when installed
make analyze       # GCC analyzer and cppcheck, when available
make debug         # Unoptimized build with SISP_DEBUG enabled
```

Shell tests use Bash; Python 3 is needed for the golden arithmetic generator.
Additional feature suites run separately:

```sh
ml/llm/run-all.sh
ml/agent/run-external-tests.sh
ml/chatbot/run-tests.sh
examples/quantum/run.sh tests
examples/sbtc/run.sh tests
```

## Repository layout

| Directory | Contents |
| --- | --- |
| `src/` | Interpreter, embedded prelude, and build helper source |
| [`lib/`](lib/README.md) | Reusable SISP modules |
| `algorithms/` | Standalone algorithm implementations |
| `examples/` | Mathematics, computer science, simulated machines, and applications |
| `research/` | Executable mathematical explorations |
| [`ml/llm/`](ml/llm/README.md) | Autodiff, neural networks, transformers, and training runners |
| [`ml/agent/`](ml/agent/README.md) | Tool-using agent experiments |
| [`ml/chatbot/`](ml/chatbot/README.md) | Native chatbot controls |
| [`ml/training/`](ml/training/README.md) | Corpus preparation and validation |
| `tests/` | Interpreter regressions and arithmetic fixtures |

C sources follow [`.clang-format`](.clang-format) (`clang-format -i src/*.c src/*.h`).
Generated executables, checkpoints, local data, and caches are excluded from Git.

# SISP library

These modules define reusable functions and load without running demonstrations
or printing output. Run programs from the repository root because `load` paths
resolve relative to the working directory.

```lisp
(load "lib/math/number-theory.lsp")
(modpow 5 117 23)   ; => 17
```

| Directory | Modules |
| --- | --- |
| `collections/` | Association lists, list operations, pairs, sequences, relations, heaps, disjoint sets |
| `graph/` | Traversal, minimum spanning trees, shortest paths, strongly connected components |
| `math/` | Number theory, combinatorics, sorting, numerical methods, nested-list matrix construction |
| `random/` | Shuffling and reservoir sampling |
| `statistics/` | Descriptive statistics |
| `text/` | Strings, search, UTF-8 characters, JSON |

Keep demonstration code in `examples/`. A library module should define its
functions without changing unrelated global state. Helpers may be global;
use module-specific names to avoid collisions when loading several modules.
Load dependencies with ordinary `load` forms.

The interpreter suite directly exercises `collections/relations.lsp`,
`collections/pairs.lsp`, `collections/sequence-extra.lsp`, and `math/matrix.lsp`.
The other modules currently have no dedicated regression suite.

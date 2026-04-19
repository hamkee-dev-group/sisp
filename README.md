Simple Symbolic Lisp-Like Interpreter

Very simple project showcasing a symbolic interpreter.
It should run on any POSIX system.

Reader contract
---------------

The reader implemented in `src/lexer.c` and `src/parser.c` supports a
deliberately small subset of Common Lisp surface syntax. This list is
authoritative: anything outside it is out of scope and is expected to be
rejected rather than silently reinterpreted.

Supported reader forms:
- `'expr`              quote shorthand
- `` `expr ``          backquote shorthand
- `,expr`              unquote (comma) shorthand, inside a backquote
- `(a b c)`            proper lists
- `(a . b)`            dotted pairs (including `(1 2 . 3)`)
- `; ...`              line comments (terminated by newline)
- `"..."`              string literals
- integer and rational number tokens (e.g. `-1`, `1/2`, `-3/4`)
- identifiers over `[A-Za-z0-9*+/<=>_#\\^-]`, case-folded to lowercase
- `{a b c}`            set literals
- `{tau : pred}`       set-builder / comprehension over the bound `tau`
- `\expr`              set-difference shorthand

Explicitly unsupported (out of scope, must stay rejected):
- Package syntax: `pkg:name`, `pkg::name`, `#:uninterned`
- `|escaped symbols|` (vertical-bar symbol quoting)
- `#` dispatch reader macros, including `#(...)` vectors, `#'fn`
  function quote, `#|...|#` block comments, `#\c` character literals,
  `#b`, `#o`, `#x`, `#S(...)`, and any other `#`-prefixed form
- All other Common Lisp reader macros not listed above

These unsupported forms must fail with a stable parser / evaluator
error; they must never be accepted as a valid datum and must never be
misread as set or comprehension syntax. The regression suite
`tests/test-reader-subset.sh` locks this contract.

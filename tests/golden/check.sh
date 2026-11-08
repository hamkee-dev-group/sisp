#!/bin/sh
# Golden arithmetic regression for the numeric tower (int / rational / float).
#
# gen.py deterministically emits two corpora:
#   arith-intrat.lsp - ~11k pure int/rational expressions
#   arith-float.lsp  - ~14k float and mixed expressions
# Their expected outputs are stored alongside as *.expected. Re-running must
# reproduce them byte-for-byte; a diff means the numeric tower changed.
#
# Expectations use bare values on stdout; REPL prompts and errors are on stderr.
set -e
here=$(dirname "$0")
sisp=${SISP:-$here/../../src/sisp}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

python3 "$here/gen.py" "$tmp/intrat.lsp" "$tmp/float.lsp" >/dev/null

fail=0
for name in intrat float; do
	# -i forces session behaviour on a pipe (as python -i does), so each
	# expression's value is echoed. The prompt goes to stderr and is dropped:
	# the expectation is pure values, one per line.
	"$sisp" -i < "$tmp/$name.lsp" > "$tmp/$name.out" 2>/dev/null
	if diff -q "$here/arith-$name.expected" "$tmp/$name.out" >/dev/null; then
		echo "PASS: arith-$name ($(wc -l < "$here/arith-$name.expected") exprs)"
	else
		echo "FAIL: arith-$name differs from golden"
		diff "$here/arith-$name.expected" "$tmp/$name.out" | head -20
		fail=1
	fi
done
exit $fail

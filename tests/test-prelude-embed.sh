#!/bin/sh
# The prelude is embedded into the binary as C string literals, so every byte
# of prelude.lsp has to survive a trip through a C compiler. That escaping was
# a sed pipeline for a long time and was wrong twice -- most recently, a lone
# backslash was not doubled, so a prelude string containing \" closed its own
# literal and broke the build, and a ??= trigraph turned into # with nothing
# but a warning. Neither was noticed, because prelude.lsp happened to contain
# neither and nothing tested the embedder.
#
# This builds a throwaway copy of src/ with a deliberately hostile prelude and
# checks that it compiles AND that each string arrives intact.

set -eu

here=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
root=$(CDPATH= cd -- "$here/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM

PASS=0
FAIL=0

check()
{
	if [ "$2" = "$3" ]
	then
		echo "PASS: $1"
		PASS=$((PASS + 1))
	else
		echo "FAIL: $1 (expected '$3', got '$2')"
		FAIL=$((FAIL + 1))
	fi
}

cp -R "$root/src" "$work/src"
cd "$work/src"

# Every character class that has to be escaped on the way into a C literal.
cat >> prelude.lsp <<'HOSTILE'
(define (embed-backslash x) "one backslash: \\ here" x)
(define (embed-quote x) "an escaped quote: \" here" x)
(define (embed-trigraph x) "trigraph??=sequence" x)
(define (embed-question x) "a question? mark" x)
HOSTILE

# cp gives every copied file the same mtime, so make cannot be trusted to
# notice the appended prelude -- main.o in particular would stay stale and the
# new header would never be compiled in. Start from nothing.
make clean >/dev/null 2>&1
if ! make >/dev/null 2>&1 || [ ! -x ./sisp ]
then
	echo "FAIL: prelude with hostile characters does not build"
	echo "TOTAL: 0 passed, 1 failed"
	exit 1
fi
echo "PASS: hostile prelude compiles"
PASS=$((PASS + 1))

# The docstrings are the payload: each must come back exactly as written.
got=$(printf '(doc embed-backslash)\n' | ./sisp -i 2>/dev/null | head -1)
check "backslash survives" "$got" 'one backslash: \ here'

got=$(printf '(doc embed-quote)\n' | ./sisp -i 2>/dev/null | head -1)
check "escaped quote survives" "$got" 'an escaped quote: " here'

# The dangerous one: ??= becomes # if the embedder does not defeat trigraphs,
# and that corrupts the standard library without failing the build.
got=$(printf '(doc embed-trigraph)\n' | ./sisp -i 2>/dev/null | head -1)
check "trigraph is not converted" "$got" 'trigraph??=sequence'

got=$(printf '(doc embed-question)\n' | ./sisp -i 2>/dev/null | head -1)
check "lone question mark survives" "$got" 'a question? mark'

# And the unmodified prelude must still embed to exactly what ships.
cp -R "$root/src" "$work/pristine"
cd "$work/pristine"
rm -f libsisp.h
make embed >/dev/null 2>&1
make libsisp.h >/dev/null 2>&1
if diff -q "$root/src/libsisp.h" libsisp.h >/dev/null 2>&1
then
	echo "PASS: regenerated libsisp.h matches the committed one"
	PASS=$((PASS + 1))
else
	echo "FAIL: regenerated libsisp.h differs from the committed one"
	FAIL=$((FAIL + 1))
fi

echo "TOTAL: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]

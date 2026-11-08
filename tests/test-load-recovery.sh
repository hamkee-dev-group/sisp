#!/bin/bash
. "$(dirname "$0")/lib.sh"
# Recovery-scope tests for nested load.
#
# These exist to pin the behaviour of nested evaluator recovery BEFORE the
# jmp_buf copies in load_isolated are replaced by a recovery-context stack.
# They were written against the copying implementation on purpose: they are
# the oracle for the replacement, so they must pass identically both sides
# of that change.
#
# What they are protecting: a nested load runs its own process loop, which
# re-runs setjmp on the evaluator recovery target. The outer form's target
# must survive that, an error inside the loaded file must unwind to the
# inner loop rather than the caller's catch, and repeated nesting must not
# leave a stale frame behind.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SISP="$SCRIPT_DIR/../src/sisp"
PASS=0
FAIL=0

if [ ! -x "$SISP" ]; then
    echo "ERROR: sisp binary not found at $SISP"
    exit 1
fi

TMP=$(mktemp -d "${TMPDIR:-/tmp}/sisp-recovery.XXXXXX")
trap 'rm -rf "$TMP"' EXIT
cd "$TMP"

# --- fixtures -----------------------------------------------------------
printf '%s\n' '(define ok-marker 1)' '(print "OK-FILE")'                  > ok.lsp
printf '%s\n' '(car 0)' '(print "AFTER-ERROR-FIRST")'                     > err-first.lsp
printf '%s\n' '(print "GOOD-FORM")' '(car 0)' '(print "AFTER-ERROR-LATER")' > err-later.lsp
printf '%s\n' '(define (spin n) (+ 1 (spin (+ n 1))))' '(spin 0)'         > deep.lsp
printf '%s\n' '(load "ok.lsp")' '(print "NEST-1-DONE")'                   > nest1.lsp
printf '%s\n' '(load "nest1.lsp")' '(print "NEST-2-DONE")'                > nest2.lsp
printf '%s\n' '(load "err-first.lsp")' '(print "UNREACHED")'              > nest-bad.lsp

run_test() {
    local name="$1" input="$2" expect="$3" reject="$4"
    output=$(printf '%s\n(quit 0)\n' "$input" | sisp_timeout 5s "$SISP" -i 2>&1)
    if [ -n "$expect" ] && ! echo "$output" | grep -q "$expect"; then
        echo "FAIL: $name (expected '$expect')"; FAIL=$((FAIL + 1)); return
    fi
    if [ -n "$reject" ] && echo "$output" | grep -q "$reject"; then
        echo "FAIL: $name (contained '$reject')"; FAIL=$((FAIL + 1)); return
    fi
    echo "PASS: $name"; PASS=$((PASS + 1))
}

echo "=== LOAD RECOVERY TESTS ==="

# A nested load, and a load nested two deep, both complete and return control.
run_test "nested-load-succeeds" \
    '(load "nest1.lsp")(print "OUTER-ALIVE")' "OUTER-ALIVE" ""
run_test "double-nested-load-succeeds" \
    '(load "nest2.lsp")(print "OUTER-ALIVE")' "NEST-2-DONE" ""

# An evaluation error inside a loaded file must abort that file and not run
# its remaining forms -- whether it is the first form or a later one.
run_test "eval-error-as-first-form-aborts-file" \
    '(catch (load "err-first.lsp") (lambda (e) (print "CAUGHT")))' \
    "CAUGHT" "AFTER-ERROR-FIRST"
run_test "eval-error-after-good-form-aborts-file" \
    '(catch (load "err-later.lsp") (lambda (e) (print "CAUGHT")))' \
    "GOOD-FORM" "AFTER-ERROR-LATER"

# The caller's own recovery target must survive the nested process loop:
# the REPL keeps going after an uncaught failure in a loaded file.
run_test "repl-alive-after-failed-load" \
    '(load "err-first.lsp")(print "REPL-ALIVE")' "REPL-ALIVE" ""

# A load failure reaches an enclosing catch as an ordinary error, and the
# handler runs in the caller, not somewhere inside the nested loop.
run_test "failed-load-reaches-enclosing-catch" \
    '(print (catch (load "err-first.lsp") (lambda (e) (car e))))' "error" ""
run_test "missing-file-reaches-enclosing-catch" \
    '(print (catch (load "no-such-file.lsp") (lambda (e) (car e))))' "error" ""

# A catch that is active when load is called must still be the handler for
# errors raised AFTER the load returns. If the nested loop leaves the handler
# stack detached or stale, this raise escapes instead of being caught.
run_test "catch-survives-a-successful-nested-load" \
    '(print (catch (progn (load "ok.lsp") (car 0)) (lambda (e) "CAUGHT-AFTER-LOAD")))' \
    "CAUGHT-AFTER-LOAD" ""
run_test "catch-survives-a-failed-nested-load" \
    '(print (catch (progn (catch (load "err-first.lsp") (lambda (e) nil)) (car 0)) (lambda (e) "CAUGHT-AFTER-FAILED-LOAD")))' \
    "CAUGHT-AFTER-FAILED-LOAD" ""

# Stack exhaustion inside a loaded file must unwind through the inner frame
# and leave the outer REPL usable. Stack exhaustion is deliberately NOT
# catchable, so the catch here must not fire.
run_test "stack-exhaustion-in-loaded-file-recovers" \
    '(load "deep.lsp")(print "ALIVE-AFTER-DEEP")' "ALIVE-AFTER-DEEP" ""

# A good load right after a failed one: the failed load must not leave the
# recovery frame pointing at something stale.
run_test "successful-load-after-failed-load" \
    '(load "err-first.lsp")(load "ok.lsp")(print "SECOND-LOAD-OK")' \
    "SECOND-LOAD-OK" ""
run_test "successful-load-after-failed-nested-load" \
    '(load "nest-bad.lsp")(load "nest1.lsp")(print "RECOVERED-NESTED")' \
    "RECOVERED-NESTED" ""

# Repeated nesting is what exposes a stale frame pointer -- one round trip
# can look fine while the tenth reuses a dead target.
run_test "repeated-nested-loads" \
    '(load "nest1.lsp")(load "nest1.lsp")(load "nest1.lsp")(load "nest1.lsp")(load "nest1.lsp")(load "nest1.lsp")(load "nest1.lsp")(load "nest1.lsp")(print "REPEAT-OK")' \
    "REPEAT-OK" ""
run_test "repeated-failing-loads" \
    '(load "err-first.lsp")(load "err-first.lsp")(load "err-first.lsp")(load "err-first.lsp")(print "REPEAT-FAIL-OK")' \
    "REPEAT-FAIL-OK" ""

# Before the recovery-frame change these two disagreed: an error in the FIRST
# form of a loaded file jumped straight into the caller's catch as the raw
# error (skipping in_load--, fclose and the buffer restore), while the same
# error one form later arrived as LOAD FAILED. Suspending the caller's handler
# for the duration of the nested loop makes position irrelevant.
run_test "failed-load-reports-consistently-first-form" \
    '(print (catch (load "err-first.lsp") (lambda (e) (cadr e))))' \
    "LOAD FAILED" '"CAR: UNDEFINED"' 
run_test "failed-load-reports-consistently-later-form" \
    '(print (catch (load "err-later.lsp") (lambda (e) (cadr e))))' \
    "LOAD FAILED" '"CAR: UNDEFINED"' 

# A command-line file whose load fails must exit non-zero, unlike the REPL
# which recovers and continues.
printf '%s\n' '(load "err-first.lsp")' '(print "UNREACHED-CLI")' > cli.lsp
out=$(sisp_timeout 5s "$SISP" cli.lsp < /dev/null 2>&1); status=$?
if [ "$status" -eq 0 ]; then
    echo "FAIL: cli-failed-load-exits-nonzero (status 0)"; FAIL=$((FAIL + 1))
elif echo "$out" | grep -q "UNREACHED-CLI"; then
    echo "FAIL: cli-failed-load-exits-nonzero (continued past failure)"; FAIL=$((FAIL + 1))
else
    echo "PASS: cli-failed-load-exits-nonzero"; PASS=$((PASS + 1))
fi

echo ""
echo "PASSED: $PASS"
echo "FAILED: $FAIL"
[ "$FAIL" -eq 0 ]

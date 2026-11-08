#!/bin/sh
# check-oracles.sh <file.lsp>
# For each  (expr...)  ; => EXPECTED  unit, load the whole file (so definitions
# are in scope), evaluate (print EXPR), and compare the printed value to EXPECTED.
# Both formats are accepted: single-line "(call) ; => x" and the two-line
# "(call)\n; => x" that batch generation emits -- a lone "; =>" line is joined
# onto its preceding call first. Reports PASS/FAIL per oracle and an exit code.
set -eu
# Repo root derived from this script's location (portable across machines).
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
file=${1:?usage: check-oracles.sh file.lsp}
base=$(basename "$file")
# Join a lone "; => x" line onto the preceding (call) line so two-line oracles
# are checkable too. The original file is copied unchanged into the test program.
norm=$(mktemp)
program=$(mktemp)
output=$(mktemp)
errors=$(mktemp)
trap 'rm -f "$norm" "$program" "$output" "$errors"' EXIT
awk '{
  if ($0 ~ /^[[:space:]]*;[[:space:]]*=>/ && buf != "") { print buf " " $0; buf="" }
  else { if (buf != "") print buf; buf=$0 }
} END { if (buf != "") print buf }' "$file" > "$norm"
pass=0; fail=0
# lines like:  (expr...)   ; => expected
matches=$(grep -nE '^\(.*\)[[:space:]]*;[[:space:]]*=>' "$norm" || true)
if [ -z "$matches" ]; then
  printf 'FAIL: %s has no checkable oracles\n' "$base"
  exit 1
fi
# Load the unit once and evaluate every oracle in that environment.  Starting a
# fresh interpreter per oracle would dominate the cost of validating a corpus of
# hundreds of files.
cp "$file" "$program"
printf '\n' >>"$program"
while IFS= read -r line; do
  no=${line%%:*}
  body=${line#*:}
  expr=$(printf '%s' "$body" | sed -E 's/[[:space:]]*;[[:space:]]*=>.*$//')
  printf '(print (list (quote oracle-result) %s %s))\n' "$no" "$expr" >>"$program"
done <<EOF
$matches
EOF
if ! "$ROOT/src/sisp" <"$program" >"$output" 2>"$errors"; then
  printf 'FAIL: %s did not complete oracle evaluation\n' "$base"
  cat "$errors"
  exit 1
fi
if [ -s "$errors" ]; then
  # A few long valid streams make the interactive reader attempt one final
  # empty parse at EOF.  Accept only that exact diagnostic; missing oracle
  # sentinels below still make malformed units fail.
  remaining=$(sed 's/; PARSER ERROR\.//g; s/[[:space:]]//g' "$errors")
  if [ -n "$remaining" ]; then
    printf 'FAIL: %s wrote interpreter errors\n' "$base"
    cat "$errors"
    exit 1
  fi
fi
while IFS= read -r line; do
  no=${line%%:*}
  body=${line#*:}
  expr=$(printf '%s' "$body" | sed -E 's/[[:space:]]*;[[:space:]]*=>.*$//')
  want=$(printf '%s' "$body" | sed -E 's/^.*;[[:space:]]*=>[[:space:]]*//' | sed -E 's/[[:space:]]+$//')
  rendered=$(grep -E "\\(oracle-result $no " "$output" | tail -1 || true)
  got=$(printf '%s' "$rendered" | sed -E \
    "s/^:?[[:space:]]*\\(oracle-result $no //; s/\\)[[:space:]]*$//")
  if [ "$got" = "$want" ]; then
    pass=$((pass+1))
  else
    fail=$((fail+1))
    printf 'FAIL L%s: %s => got[%s] want[%s]\n' "$no" "$expr" "$got" "$want"
  fi
done <<EOF
$matches
EOF
printf '=== %s: %s pass, %s fail ===\n' "$base" "$pass" "$fail"
[ "$fail" = 0 ]

#!/bin/sh
# Portable wall-clock limit for macOS and Linux; exits 124 on timeout.  Python is
# already required by corpus preparation and avoids GNU-timeout portability gaps.
set -eu

seconds=${1:?usage: run-with-timeout.sh seconds command [args...]}
shift
case "$seconds" in
  ''|*[!0-9]*) printf 'timeout must be a positive integer\n' >&2; exit 2 ;;
esac
if [ "$seconds" -eq 0 ] || [ "$#" -eq 0 ]; then
  printf 'timeout and command must be non-empty\n' >&2
  exit 2
fi

exec python3 -c '
import subprocess
import sys

seconds = int(sys.argv[1])
command = sys.argv[2:]
try:
    completed = subprocess.run(command, timeout=seconds)
except subprocess.TimeoutExpired:
    print(f"command exceeded {seconds} seconds: {command[0]}", file=sys.stderr)
    raise SystemExit(124)
raise SystemExit(completed.returncode)
' "$seconds" "$@"

# Shared helpers for the SISP shell test scripts.
#
# sisp_timeout SECONDS CMD...  — run CMD under a wall-clock limit, portably.
# Prefers timeout/gtimeout; falls back to python3 (already a repo dependency),
# so a hang is still caught and the suite is honestly green on macOS and Linux
# alike, where a bare `timeout` may be absent.
sisp_timeout() {
    _t=$1
    shift
    if command -v timeout >/dev/null 2>&1; then
        timeout "$_t" "$@"
    elif command -v gtimeout >/dev/null 2>&1; then
        gtimeout "$_t" "$@"
    else
        _secs=$(printf '%s' "$_t" | tr -dc '0-9')
        python3 -c 'import subprocess, sys
try:
    r = subprocess.run(sys.argv[2:], timeout=int(sys.argv[1]))
except subprocess.TimeoutExpired:
    sys.exit(124)
sys.exit(r.returncode)' "$_secs" "$@"
    fi
}

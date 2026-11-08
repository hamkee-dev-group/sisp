#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
python3 ml/agent/check-protocol-data.py "$@"

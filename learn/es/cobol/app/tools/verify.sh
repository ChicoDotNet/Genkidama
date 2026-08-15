#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
bash tests/smoke.sh
printf 'NominaBatch verify: OK\n'

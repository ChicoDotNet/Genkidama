#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
printf '== NominaBatch: smoke funcional ==\n'
bash tests/smoke.sh
printf '\n== NominaBatch: fallos operativos ==\n'
bash tests/operational.sh
printf '\nNominaBatch verify: OK\n'

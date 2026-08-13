#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
forge fmt --check app/src/FreelanceEscrow.sol app/test/FreelanceEscrow.t.sol
forge build
forge test -vv

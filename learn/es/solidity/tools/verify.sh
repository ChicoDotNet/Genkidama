#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../app"
forge fmt --check
forge build
forge test -vv

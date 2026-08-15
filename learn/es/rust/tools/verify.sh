#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../app"

cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo build --release

#!/usr/bin/env bash
set -euo pipefail

standard() { echo $(( $1 * 100 )); }
discounted() { echo $(( $1 * 80 )); }
price_with() { local strategy=$1 qty=$2; "$strategy" "$qty"; }
[[ $(price_with standard 2) == 200 ]]
[[ $(price_with discounted 2) == 160 ]]

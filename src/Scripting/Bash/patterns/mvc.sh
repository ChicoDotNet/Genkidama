#!/usr/bin/env bash
set -euo pipefail

model_count=0
model_increment() { model_count=$((model_count+1)); }
view_render() { echo "Count=$model_count"; }
controller_click() { model_increment; view_render; }
[[ $(controller_click) == "Count=1" ]]

#!/usr/bin/env bash
set -euo pipefail

model_total=10
view_text=""
view_set_total() { view_text="Total=$1"; }
presenter_refresh() { view_set_total "$model_total"; }
presenter_refresh
[[ $view_text == "Total=10" ]]
model_total=12
presenter_refresh
[[ $view_text == "Total=12" ]]

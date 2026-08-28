#!/usr/bin/env bash
set -euo pipefail

declare -A table=()
record_id=7
record_name="Ada"
record_save() { table["$record_id"]=$record_name; }
record_find() { record_id=$1; record_name=${table[$1]}; }
record_save
record_name=""
record_find 7
[[ $record_name == Ada ]]

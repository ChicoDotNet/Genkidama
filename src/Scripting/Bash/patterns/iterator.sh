#!/usr/bin/env bash
set -euo pipefail

declare -a values=(alpha beta gamma)
index=0
has_next() { (( index < ${#values[@]} )); }
next_item() {
  NEXT_ITEM=${values[index]}
  index=$((index+1))
}
seen=()
while has_next; do
  next_item
  seen+=("$NEXT_ITEM")
done
[[ ${seen[*]} == "alpha beta gamma" ]]
[[ $index -eq 3 ]]

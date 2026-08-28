#!/usr/bin/env bash
set -euo pipefail

state="draft"
save_memento() { MEMENTO=$state; }
restore_memento() { state=$MEMENTO; }
save_memento
state="published"
restore_memento
[[ $state == draft ]]

#!/usr/bin/env bash
set -euo pipefail

document="draft"
declare -a views=()
attach_view() { views+=("$1"); }
render_editor() { EDITOR_VIEW="editor:$document"; }
render_preview() { PREVIEW_VIEW="preview:$document"; }
refresh_views() { local v; for v in "${views[@]}"; do "render_$v"; done; }
attach_view editor
attach_view preview
document="final"
refresh_views
[[ $EDITOR_VIEW == editor:final ]]
[[ $PREVIEW_VIEW == preview:final ]]

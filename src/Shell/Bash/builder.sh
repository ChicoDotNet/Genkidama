#!/usr/bin/env bash
set -euo pipefail

TEXT_PARTS=()
HTML_PARTS=()

text_reset() { TEXT_PARTS=(); }
text_add_title() { TEXT_PARTS+=("# $1"); }
text_add_section() { TEXT_PARTS+=("## $1" "$2"); }
text_build() { printf '%s\n' "${TEXT_PARTS[@]}"; }

html_reset() { HTML_PARTS=(); }
html_add_title() { HTML_PARTS+=("<h1>$1</h1>"); }
html_add_section() { HTML_PARTS+=("<h2>$1</h2>" "<p>$2</p>"); }
html_build() { local IFS=''; printf '%s\n' "${HTML_PARTS[*]}"; }

build_availability_report() {
  local family="$1"
  "${family}_reset"
  "${family}_add_title" "Service status"
  "${family}_add_section" "Availability" "99.95%"
  "${family}_build"
}

build_availability_report text
printf '%s\n' '---'
build_availability_report html

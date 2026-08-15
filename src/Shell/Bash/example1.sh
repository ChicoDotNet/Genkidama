#!/usr/bin/env bash
set -euo pipefail

# Concrete products
dark_button() {
    echo "Dark Button"
}

light_button() {
    echo "Light Button"
}

dark_checkbox() {
    echo "Dark Checkbox"
}

light_checkbox() {
    echo "Light Checkbox"
}

# Concrete factories. Each associative array keeps one coherent family together.
declare -A dark_factory=(
    [create_button]=dark_button
    [create_checkbox]=dark_checkbox
)

declare -A light_factory=(
    [create_button]=light_button
    [create_checkbox]=light_checkbox
)

create_ui_components() {
    local factory_name=$1
    local -n factory=$factory_name

    "${factory[create_button]}"
    "${factory[create_checkbox]}"
}

# Usage: select the family once, then request all related products through it.
create_ui_components dark_factory
create_ui_components light_factory

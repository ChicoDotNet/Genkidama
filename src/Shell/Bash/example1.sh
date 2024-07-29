#!/bin/bash

# Abstract Factory
create_button() {
    local theme=$1
    if [ "$theme" == "dark" ]; then
        dark_button
    elif [ "$theme" == "light" ]; then
        light_button
    fi
}

create_checkbox() {
    local theme=$1
    if [ "$theme" == "dark" ]; then
        dark_checkbox
    elif [ "$theme" == "light" ]; then
        light_checkbox
    fi
}

# Concrete Products
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

# Usage
create_button "dark"
create_checkbox "light"

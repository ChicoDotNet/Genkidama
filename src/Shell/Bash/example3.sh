#!/bin/bash

# Abstract Factory
create_report() {
    local report_type=$1
    if [ "$report_type" == "pdf" ]; then
        generate_pdf_report
    elif [ "$report_type" == "html" ]; then
        generate_html_report
    fi
}

# Concrete Implementations
generate_pdf_report() {
    echo "Generating PDF report"
}

generate_html_report() {
    echo "Generating HTML report"
}

# Usage
create_report "pdf"
create_report "html"

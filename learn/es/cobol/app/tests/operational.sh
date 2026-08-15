#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
rm -f nomina
cobc -x -free -Wall -I copybooks -o nomina src/nomina.cob

workspace="$(mktemp -d)"
trap 'rm -rf "$workspace"' EXIT

missing_input="$workspace/missing-input"
mkdir -p "$missing_input"
cp nomina "$missing_input/"
set +e
missing_output="$(cd "$missing_input" && ./nomina 2>&1)"
missing_status=$?
set -e
test "$missing_status" -eq 2
printf '%s\n' "$missing_output" | grep -F "ERROR|EMPLOYEE_OPEN|STATUS="

report_failure="$workspace/report-failure"
mkdir -p "$report_failure/data" "$report_failure/report.txt"
cp nomina "$report_failure/"
cp data/employees.dat "$report_failure/data/"
set +e
report_output="$(cd "$report_failure" && ./nomina 2>&1)"
report_status=$?
set -e
test "$report_status" -eq 3
printf '%s\n' "$report_output" | grep -F "ERROR|REPORT_OPEN|STATUS="

printf 'NominaBatch operational failures: OK\n'

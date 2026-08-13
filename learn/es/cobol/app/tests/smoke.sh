#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
rm -f nomina report.txt
cobc -x -free -Wall -o nomina src/nomina.cob
./nomina

grep -F "E001|Empleado Uno|10000.00|1000.00|9000.00" report.txt
grep -F "RECHAZADO|E003|HORAS fuera de rango 1..80" report.txt
grep -F "RESUMEN|PROCESADOS=2|RECHAZADOS=1" report.txt
printf 'NominaBatch smoke: OK\n'

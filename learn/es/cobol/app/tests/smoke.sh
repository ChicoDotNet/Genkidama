#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
rm -f nomina report.txt
cobc -x -free -Wall -I copybooks -o nomina src/nomina.cob
./nomina

grep -F "E001|Demo Uno|10000.00|1000.00|9000.00" report.txt
grep -F "E002|Demo Dos|6317.50|505.40|5812.10" report.txt
grep -F "RECHAZADO|E003|HORAS fuera de rango 1..80" report.txt
grep -F "RECHAZADO|E004|FORMATO: se esperaban 5 campos" report.txt
grep -F "RECHAZADO|E005|HORAS no es numérico" report.txt
grep -F "RECHAZADO|E006|TARIFA debe ser mayor que cero" report.txt
grep -F "RECHAZADO|E007|DEDUCCIÓN debe estar entre 0 y 100" report.txt
grep -F "RECHAZADO||ID vacío" report.txt
grep -F "RESUMEN|PROCESADOS=2|RECHAZADOS=6|BRUTO=16317.50|DEDUCCIONES=1505.40|NETO=14812.10" report.txt
printf 'NominaBatch smoke: OK\n'

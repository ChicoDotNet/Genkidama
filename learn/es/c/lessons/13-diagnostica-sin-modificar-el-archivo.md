# Lección 13 — Diagnostica sin modificar el archivo

## Qué vas a conseguir

Añadirás una capacidad operacional de solo lectura para saber cuántos registros siguen siendo válidos y dónde termina el prefijo confiable de un `.gtl`.

## El problema

`list` y `summary` fallan correctamente ante corrupción, pero para soporte necesitas evidencia antes de decidir qué hacer. Un diagnóstico útil no debe "arreglar" nada por sorpresa.

## Concepto

`telemetry_diagnose_file` separa dos resultados: si la inspección pudo ejecutarse y cuál fue el primer defecto del stream. `telemetry_diagnostics` conserva conteo, bytes válidos y timestamps del prefijo sano.

[EJECUTAR]

```bash
./app/build/telemetry_cli diagnose sample.gtl
```

Un archivo sano reporta `Estado: ok`. Uno truncado reporta `truncated_record` y conserva el tamaño del prefijo válido.

## Buenas prácticas

- diagnóstico antes de remediación;
- salida estructurada y estable;
- no modificar el archivo observado;
- distinguir error de I/O de error de contenido.

## Tu turno

Provoca una copia truncada y compara `diagnose` con `list`.

## Siguiente paso

En la [lección 14](14-depura-con-evidencia-reproducible.md) usarás esas señales para depurar sin adivinar.

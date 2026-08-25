# Lección 14 — Depura con evidencia reproducible

## Qué vas a conseguir

Practicarás un ciclo de debugging reproducible sobre TelemetryTape: reproducir, observar, reducir, probar y reparar.

## El problema

Un archivo binario corrupto puede producir síntomas parecidos con causas distintas. Cambiar código antes de identificar `invalid_format`, `unsupported_version` o `truncated_record` crea reparaciones frágiles.

## Método

1. conserva una copia del fixture que falla;
2. ejecuta `diagnose` y registra el resultado;
3. reduce el caso al mínimo que reproduce la falla;
4. añade una prueba de regresión;
5. cambia una sola frontera;
6. ejecuta GCC y Clang con warnings como errores.

La suite del curso usa exactamente esta disciplina para truncado, versión futura, filtros y recuperación.

## Tu turno

Construye un archivo con header válido y tres bytes extra. Explica por qué el resultado correcto es `truncated_record` y no `invalid_format`.

## Siguiente paso

En la [lección 15](15-recupera-sin-sobrescribir-el-original.md) convertirás el diagnóstico en una recuperación explícita y reversible.

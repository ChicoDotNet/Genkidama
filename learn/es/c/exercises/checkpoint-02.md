# Checkpoint 02 — Analiza sin perder control de memoria

Trabaja sobre TelemetryTape sin mirar la solución.

## Objetivo

Añade un comando `count` a la CLI que imprima únicamente el número de registros completos del archivo.

## Reglas

1. Debe reutilizar `telemetry_count_records`; no copies el parser binario a `main.c`.
2. Debe devolver código 0 cuando el archivo válido está vacío.
3. Debe fallar con el diagnóstico existente para versión no soportada o registro truncado.
4. No debe reservar un arreglo de registros.
5. Añade al menos una comprobación automatizada o amplía el smoke de CI para demostrar el comportamiento.

## Evidencia esperada

```text
$ telemetry_cli count sample.gtl
3
```

Si el archivo está truncado, no debe imprimir un conteo parcial como si fuera éxito.

Cuando termines, compara tu solución con [la referencia](../solutions/checkpoint-02.md).

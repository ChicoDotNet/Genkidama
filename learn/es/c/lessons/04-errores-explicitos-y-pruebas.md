# Lección 04 — Haz explícitos los errores y prueba comportamiento

## Qué vas a conseguir

Distinguirás error de argumento, I/O, formato y capacidad, y ejecutarás pruebas que siguen activas incluso en Release.

## El problema

En C no hay una excepción automática que transporte contexto. Devolver `false` para todo tampoco ayuda: el llamador necesita saber si debe corregir un argumento, revisar el filesystem o rechazar datos corruptos.

## Concepto

La API devuelve `telemetry_result`. El llamador decide cómo presentar el error y la biblioteca conserva un contrato pequeño, estable y testeable.

## Pruebas Release de verdad

El runner usa `CHECK`, no `assert`. `assert` es útil para invariantes de desarrollo, pero puede desaparecer cuando se define `NDEBUG`; no queremos que el build Release “pase” porque dejó de ejecutar expresiones de prueba.

[EJECUTAR]

```bash
ctest --test-dir app/build --output-on-failure
```

La suite demuestra round-trip, header corrupto, capacidad insuficiente y argumentos inválidos.

## Tu turno

Corrompe deliberadamente una copia del header y ejecuta `list`. Debe fallar y nunca reemplazar el archivo.

Después resuelve el [Checkpoint 01 — Archivo confiable](../exercises/checkpoint-01.md).

## Siguiente paso

Continúa con [Lección 05 — Cuenta antes de reservar memoria](05-cuenta-antes-de-reservar-memoria.md).

## Referencias

- CMake/CTest documentation.
- CERT C como referencia complementaria de manejo defensivo; este curso prioriza primero el estándar y contratos locales.

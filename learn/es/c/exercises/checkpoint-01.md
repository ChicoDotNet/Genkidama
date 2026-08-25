# Checkpoint 01 — Archivo confiable

Trabaja sobre **TelemetryTape**, no sobre un programa nuevo.

## Misión

Agrega a la API pública una función:

```c
telemetry_result telemetry_count_records(const char *path, size_t *out_count);
```

Debe contar registros completos **sin copiar sus payloads** y respetar el mismo header/versionado que el lector.

## Criterios observables

- Un archivo recién creado devuelve 0.
- Dos registros válidos devuelven 2.
- Un header incorrecto devuelve `TELEMETRY_INVALID_FORMAT`.
- Un registro final truncado devuelve `TELEMETRY_INVALID_FORMAT`.
- `path == NULL` u `out_count == NULL` devuelve `TELEMETRY_INVALID_ARGUMENT`.
- La función pública queda documentada en `telemetry.h`.
- Añade pruebas que sigan funcionando en Release.

## Restricciones

No leas todo el archivo a memoria sólo para contar y no copies la lógica de validación del header fuera de la biblioteca.

[PAUSA PARA EJERCICIO]

Cuando termines, ejecuta build + CTest. Sólo después compara con la [solución de referencia](../solutions/checkpoint-01.md).

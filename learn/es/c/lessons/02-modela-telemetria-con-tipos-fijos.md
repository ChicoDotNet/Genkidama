# Lección 02 — Modela telemetría con tipos de ancho fijo

## Qué vas a conseguir

Entenderás el primer contrato de dominio de TelemetryTape y registrarás una muestra con unidades explícitas.

## El problema

`int` no garantiza el mismo ancho en todas las plataformas y `float`/`double` complican un formato binario durable. Para un archivo necesitamos decidir cuántos bits ocupa cada campo y qué significa su unidad.

## Concepto

`telemetry_record` usa tipos de `<stdint.h>`:

- `int64_t timestamp_ms`: milisegundos desde una referencia acordada;
- `uint32_t sensor_id`: identificador positivo;
- `int32_t value_milli`: valor escalado por mil;
- `uint8_t status`: código pequeño de estado.

No escribimos la `struct` completa con `fwrite(&record, sizeof record, 1, file)`: padding, endianness y representación harían que el archivo dependiera de la ABI.

[DEMO]

```bash
./app/build/telemetry_cli log sample.gtl 1000 7 21500 0
./app/build/telemetry_cli list sample.gtl
```

## Qué acaba de pasar

La CLI convierte texto a enteros, valida rangos y construye un valor `telemetry_record`. La biblioteca recibe un puntero `const` porque no necesita modificar el registro del llamador.

## Tu turno

Agrega dos muestras con sensores distintos, una con `value_milli` negativo. Luego lista el archivo y explica qué unidad tendría sentido en tu escenario (por ejemplo, miligrados o milivoltios).

## Buenas prácticas

- Unidades en nombres o documentación.
- Tipos con ancho fijo cuando el formato externo lo requiere.
- `const` para entradas no mutadas.
- Rangos validados antes de convertir a tipos más pequeños.

## Siguiente paso

[Lección 03 — Diseña un formato binario portable](03-disena-un-formato-binario-portable.md)

## Referencias

- GCC C23 status.
- ISO C fixed-width integer types mediante `<stdint.h>`.

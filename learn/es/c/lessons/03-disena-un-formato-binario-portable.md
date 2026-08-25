# Lección 03 — Diseña un formato binario portable

## Qué vas a conseguir

Leerás cómo TelemetryTape transforma valores a bytes con un formato explícito y versionado.

## El problema

Un archivo binario rápido de escribir puede convertirse en deuda si nadie sabe reconocer su versión, su byte order o el tamaño de un registro.

## Formato v1

El archivo comienza con cuatro bytes:

```text
G T L 01
```

Cada registro ocupa exactamente **17 bytes**:

```text
0..7    timestamp_ms  uint64 little-endian
8..11   sensor_id     uint32 little-endian
12..15  value_milli   uint32 bit-pattern little-endian
16      status        uint8
```

Las funciones privadas `write_u32_le`, `write_u64_le`, `read_u32_le` y `read_u64_le` hacen el contrato visible. La serialización no depende del padding de la `struct`.

## Fallos importantes

- Un magic/version incorrecto devuelve `TELEMETRY_INVALID_FORMAT`.
- Un registro truncado también es formato inválido.
- `append` valida el header existente: jamás “arregla” un archivo corrupto truncándolo silenciosamente.

## Tu turno

Con una herramienta hexadecimal disponible en tu sistema, inspecciona `sample.gtl`. Identifica los cuatro bytes de header y los 17 del primer registro. No necesitas memorizar hexadecimal: relaciona campos con posiciones.

## Reflexión

¿Por qué sería peligroso cambiar el orden de campos sin incrementar la versión? Porque lectores antiguos interpretarían bytes válidos con significado incorrecto, un fallo peor que rechazar explícitamente el archivo.

## Siguiente paso

[Lección 04 — Haz explícitos los errores y prueba comportamiento](04-errores-explicitos-y-pruebas.md)

## Referencias

- Documentación de C sobre `<stdint.h>` y operaciones de archivos de `<stdio.h>`.

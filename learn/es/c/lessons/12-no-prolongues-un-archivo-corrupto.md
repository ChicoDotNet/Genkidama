# Lección 12 — No prolongues un archivo corrupto

## Qué vas a conseguir

Endurecerás la escritura para que TelemetryTape nunca agregue un registro al final de un stream que ya está truncado o semánticamente inválido.

## El problema

Validar sólo los cuatro bytes del header antes de `append` no alcanza. Un archivo puede tener header correcto y terminar con tres bytes de un registro incompleto. Agregar 17 bytes después no repara nada: hace más difícil diagnosticar el daño.

## Concepto

Antes de abrir en modo append, `telemetry_append_record` recorre el archivo usando el mismo parser de lectura. Sólo si el stream completo es válido abre `ab` y agrega el nuevo registro.

Es una elección deliberada: **integridad primero**. Para cargas extremas podrían diseñarse checksums, índices o una estrategia transaccional diferente, pero no debemos fingir que el header prueba todo el archivo.

## Failure modes protegidos

La suite verifica ahora:

- filtro por sensor;
- intervalo `[start,end)`;
- filtro inválido;
- CSV determinista;
- rechazo de append sobre registro truncado.

[EJECUTAR]

```bash
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure
```

## Tu turno

Resuelve el [Checkpoint 03 — Consulta sin romper el contrato](../exercises/checkpoint-03.md) sin consultar la solución primero.

## Siguiente paso

En la [lección 13](13-diagnostica-sin-modificar-el-archivo.md) convertirás los mismos errores explícitos en evidencia operacional sin modificar el archivo observado.

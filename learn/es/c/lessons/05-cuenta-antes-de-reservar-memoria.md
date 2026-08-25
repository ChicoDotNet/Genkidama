# Lección 05 — Cuenta antes de reservar memoria

## Qué vas a conseguir

Eliminarás el arreglo fijo de 256 registros de la CLI y reservarás exactamente la memoria que necesita el archivo.

## El problema

Un buffer fijo parece simple, pero convierte una decisión arbitraria en un límite del producto. Si el archivo contiene 257 registros, `list` falla aunque el equipo tenga memoria suficiente.

## Concepto

En C, quien reserva memoria debe tener claro cuánto necesita y quién la libera. `telemetry_count_records` recorre el archivo sin construir un arreglo. La CLI usa ese conteo, valida `SIZE_MAX / sizeof(telemetry_record)`, llama `malloc` y libera con `free` en todos los caminos.

[DEMO]

```bash
./build/telemetry_cli list sample.gtl
```

La biblioteca sigue sin decidir la política de memoria del ejecutable: `telemetry_read_records` recibe un buffer y su capacidad. Esa separación permite usar la misma API con memoria dinámica, un arreglo estático pequeño o un buffer administrado por otro sistema.

## Errores comunes

- multiplicar `count * sizeof(record)` sin validar overflow;
- perder el puntero antes de `free`;
- asumir que `malloc(0)` debe devolver NULL o no-NULL;
- olvidar liberar en una rama de error.

## Tu turno

Agrega varios registros y comprueba que `list` reporta el conteo exacto. Después explica con tus palabras quién es dueño del arreglo en la CLI.

## Siguiente paso

[Lección 06 — Resume sin cargar todo el archivo](06-resume-sin-cargar-todo.md).

## Referencias

- ISO C dynamic memory functions (`malloc`, `free`).
- GCC C language documentation.

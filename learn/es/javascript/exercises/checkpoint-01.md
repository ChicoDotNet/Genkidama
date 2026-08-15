# Checkpoint 01 — Límite de trabajo en curso

## Historia
El equipo quiere evitar demasiadas tareas simultáneas. La columna `doing` debe aceptar **como máximo 3 tarjetas**.

## Tu trabajo
Modifica la lógica para que intentar mover una cuarta tarjeta a `doing` lance un error claro y no altere el tablero original. No hay receta de implementación.

## Criterios
- `todo` y `done` siguen funcionando;
- hasta 3 tarjetas pueden estar en `doing`;
- la cuarta produce un error explícito;
- el tablero recibido no se muta al fallar;
- agrega al menos una prueba automatizada.

## Comprobar
Desde `app/`, ejecuta `npm test`.

Cuando tengas tu solución, compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

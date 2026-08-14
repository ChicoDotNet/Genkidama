# Solución de referencia — Checkpoint 04

Una solución pequeña mantiene la nueva señal dentro de `RequestMetrics`, porque clasificar status HTTP pertenece a la observabilidad del adaptador y no al dominio.

## Cambio de estado

Agrega un contador privado:

```ts
private serverErrors = 0;
```

Incluye `serverErrors` en `DiagnosticsSnapshot` y en `snapshot()`.

En `record` conserva el contador existente y agrega la condición específica:

```ts
if (statusCode >= 400) this.failedRequests += 1;
if (statusCode >= 500 && statusCode <= 599) this.serverErrors += 1;
```

No necesitas almacenar la petición que produjo el status.

## Prueba mínima

Una prueba equivalente puede hacer:

```ts
const metrics = new RequestMetrics();
metrics.record(200, 2);
metrics.record(400, 3);
metrics.record(503, 5);

assert.deepEqual(metrics.snapshot(), {
  totalRequests: 3,
  failedRequests: 2,
  serverErrors: 1,
  totalDurationMs: 10,
  maxDurationMs: 5,
});
```

Después ejecuta `npm run verify` y confirma que las regresiones HTTP continúan verdes.

## Por qué

`serverErrors` permite separar rápidamente fallas del servicio de errores de entrada, pero sigue siendo un agregado: no identifica si el `503` provino de disco, red u otra dependencia. Para responder esa segunda pregunta haría falta instrumentar una frontera concreta, manteniendo el mismo principio de mínima recolección necesaria.

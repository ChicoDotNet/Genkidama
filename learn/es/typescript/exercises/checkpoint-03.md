# Checkpoint 03 — Cotización enviada sin estado fantasma

Trabaja sobre FreelanceDesk sin abrir la solución.

## Escenario

El flujo comercial ya permite `draft → sent → accepted|rejected`. Ahora protege una condición operativa: si la persistencia falla mientras una cotización pasa de `draft` a `sent`, el proceso debe conservar `draft`.

## Requisitos

1. Crea una cotización válida.
2. Configura un `AppStateStore` de prueba que falle exactamente en el siguiente `save`.
3. Solicita `PATCH /api/quotes/:id/status` con `sent`.
4. Comprueba respuesta `503`.
5. Consulta de nuevo la cotización y demuestra que continúa `draft`.
6. Después permite una escritura normal y confirma que `draft → sent` sí queda persistido.
7. No debilites `changeQuoteStatus`, no captures el error dentro del store falso y no mutues manualmente el estado desde la prueba.

## Reflexión

En 120–200 palabras explica por qué este comportamiento es distinto de una transacción distribuida y qué riesgo seguiría existiendo si dos procesos escribieran el mismo archivo JSON.

## Comprobación

```bash
npm run verify
```

Cuando termines, compara con [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).

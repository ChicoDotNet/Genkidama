# Checkpoint 04 — Diagnóstico sin filtrar datos

Trabaja sobre `RequestMetrics` después de completar la Lección 16. No abras la solución antes de intentar el cambio.

## Problema

El contador actual agrupa todos los status `>=400` como `failedRequests`. Para operar una API conviene distinguir errores provocados por una petición inválida de fallas `5xx` que requieren atención del servicio.

Extiende el diagnóstico para exponer también:

```text
serverErrors
```

## Contrato

Tu implementación debe cumplir simultáneamente:

- un status `400`, `404`, `413` o `415` incrementa `failedRequests` pero no `serverErrors`;
- un status entre `500` y `599` incrementa ambos contadores;
- un status exitoso no incrementa ninguno;
- `snapshot()` sigue devolviendo una copia inmutable;
- no almacenes URL, método, body, email, IDs ni mensajes de error;
- no cambies el dominio para conseguir la métrica.

## Prueba obligatoria

Añade una prueba determinista que registre al menos un `200`, un `400` y un `503`, y compruebe el snapshot completo.

Después ejecuta:

```bash
npm run verify
```

## Reflexión

Explica en dos o tres frases por qué `serverErrors` ayuda a decidir dónde investigar, pero no demuestra por sí solo la causa de una falla.

Cuando termines, compara tu solución con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).

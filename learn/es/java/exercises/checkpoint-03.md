# Checkpoint 03 — Diagnóstico que falla de forma útil

Trabaja sobre HelpDesk después de la lección 12. No abras la solución hasta completar un intento.

## Escenario

Operación quiere distinguir respuestas sanas de fallos internos, pero seguridad no permite registrar contenido de tickets ni URLs completas.

Implementa una regresión que use un `TicketStore` cuya escritura siempre lance `TicketPersistenceException` y demuestra, por HTTP, que:

1. `POST /api/tickets` devuelve `503`;
2. el ticket rechazado no aparece en el resumen;
3. con diagnóstico habilitado, `failures` aumenta exactamente por la respuesta 5xx;
4. el snapshot diagnóstico no contiene el título ni la descripción enviados.

## Restricciones

- No cambies el dominio para conocer HTTP.
- No captures request bodies dentro de `RequestMetrics`.
- No conviertas 4xx en fallos del servidor.
- No uses `sleep` para coordinar la prueba.
- No desactives ni debilites pruebas existentes.

## Evidencia

Entrega:

```bash
mvn verify
```

y explica en pocas líneas por qué un contador 5xx es información operativa y no una auditoría completa.

## Reto

¿Qué cambiarías para medir latencia por rangos (`<10 ms`, `<100 ms`, `>=100 ms`) sin guardar una duración por request?

Después de tu intento consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).

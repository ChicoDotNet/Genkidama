# Checkpoint 02 — Reprogramar sin perder consistencia

Trabaja sobre AgendaPHP después de la Lección 08. No abras la solución antes de intentar el cambio.

## Escenario

El negocio quiere una operación `reschedule(id, startsAt, durationMinutes)` que cambie **sólo horario y duración**. Cliente, servicio e ID deben conservarse.

## Requisitos

1. Un ID inexistente debe producir un error explícito.
2. La reprogramación debe reutilizar las reglas actuales de formato, duración y solapamiento.
3. Si el nuevo horario choca con otra cita, el calendario durable debe quedar exactamente como estaba.
4. Una reprogramación válida conserva `id`, `clientName` y `serviceName`.
5. Añade al menos dos pruebas: éxito y conflicto sin mutación.
6. No implementes la regla en `public/index.php` ni accedas al archivo JSON desde el servicio.

## Evidencia esperada

```bash
cd app
vendor/bin/phpunit
```

Después explica por qué esta operación puede implementarse encima de `AppointmentStore` sin cambiar la persistencia.

## Reflexión

¿En qué momento una operación de reprogramación justificaría una transacción SQL? Distingue “quiero usar SQLite” de “necesito atomicidad/queries concurrentes que el store actual ya no puede ofrecer”.

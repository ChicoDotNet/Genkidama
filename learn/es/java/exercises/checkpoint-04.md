# Checkpoint 04 — Rechazo temprano sin mutación

Trabaja sobre HelpDesk después de la Lección 16. No abras la solución antes de intentar el checkpoint.

## Escenario

Un cliente integra la API con un proxy que accidentalmente envía `Content-Type: text/plain` al cambiar la prioridad de un ticket. El equipo quiere demostrar que una petición incompatible no puede modificar ni persistir estado.

## Encargo

1. Crea un ticket con prioridad `HIGH`.
2. Envía `PUT /api/tickets/{id}/priority` con body JSON válido pero media type `text/plain`.
3. Exige respuesta `415`.
4. Vuelve a consultar el ticket y demuestra que sigue en `HIGH`.
5. Conserva `mvn verify` verde.
6. Explica en 4–6 líneas por qué esta validación pertenece al adaptador HTTP y no a `TicketService`.

## Restricciones

- No debilites el parser ni las reglas del dominio.
- No captures ni imprimas el body para diagnosticar.
- No introduzcas un framework nuevo para resolver el ejercicio.
- La prueba debe observar comportamiento, no campos privados.

## Evidencia mínima

Entrega la prueba, el comando ejecutado y una explicación de la frontera elegida.

## Después de intentarlo

Compara con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).

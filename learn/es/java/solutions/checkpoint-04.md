# Solución de referencia — Checkpoint 04

Una solución razonable extiende `HelpDeskHttpServerTest` sin tocar `TicketService`.

## Dirección

Crea primero un ticket `HIGH` con `application/json`. Después envía el mismo JSON de actualización de prioridad con `Content-Type: text/plain` y espera `415`. Finalmente consulta el ticket o el listado y comprueba que la prioridad continúa en `HIGH`.

La razón arquitectónica es que el media type describe **cómo llegó el mensaje por HTTP**. `TicketService` recibe valores Java ya interpretados y no debería conocer headers, sockets ni protocolos. Si la petición no satisface el contrato de transporte, se rechaza antes de invocar dominio o persistencia.

Una prueba de referencia puede seguir esta forma:

```java
HttpResponse<String> wrongType = send(
        server,
        "PUT",
        "/api/tickets/1/priority",
        "text/plain",
        "{\"priority\":\"LOW\"}");
assertEquals(415, wrongType.statusCode());

JsonNode ticket = json.readTree(get(server, "/api/tickets").body()).get(0);
assertEquals("HIGH", ticket.get("priority").asText());
```

Los nombres auxiliares pueden variar. Evalúa el comportamiento, no la similitud exacta con esta referencia.

## Comprobación

```bash
mvn verify
```

Si la petición incompatible cambia el ticket aunque la respuesta sea 415, el checkpoint no está resuelto: el rechazo llegó demasiado tarde.

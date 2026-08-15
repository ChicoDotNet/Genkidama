# Lección 03 — Una API HTTP real con JSON

## Qué vas a conseguir

Vas a exponer el dominio por HTTP sin mover las reglas al servidor web. Crearás/listarás tickets JSON y probarás la aplicación contra un socket real.

## Antes de empezar

Completa la [Lección 02](02-reglas-dominio-y-ciclo.md).

## El problema

Un equipo cliente no puede invocar directamente `TicketService`. Necesita una frontera de integración. HTTP es común en trabajo backend Java, pero un framework demasiado temprano puede esconder qué entra, qué sale y dónde ocurren los errores.

## Concepto

El primer adaptador usa `HttpServer` del JDK y Jackson únicamente para JSON. Esa dependencia está justificada porque escribir un parser JSON casero sería menos profesional y desviaría la lección.

La dirección permanece:

```text
HTTP/JSON -> HelpDeskHttpServer -> TicketService -> Ticket
```

`CreateTicketRequest` es un DTO de frontera; `Ticket` sigue siendo dominio.

## Demostración

[EJECUTAR]

```bash
mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
```

En otra terminal:

```bash
curl http://localhost:8080/health
curl -X POST http://localhost:8080/api/tickets \
  -H 'Content-Type: application/json' \
  -d '{"title":"VPN caída","description":"Sin acceso remoto","priority":"HIGH"}'
curl http://localhost:8080/api/tickets
```

## Código real

El handler de colección traduce protocolo a dominio:

```java
CreateTicketRequest request = json.readValue(
        exchange.getRequestBody(),
        CreateTicketRequest.class);
Ticket created = tickets.create(
        request.title(),
        request.description(),
        request.priority());
send(exchange, 201, created);
```

No valida de nuevo la longitud del título: esa regla ya pertenece al dominio.

La prueba `HelpDeskHttpServerTest` pide puerto `0`, deja que el sistema operativo asigne uno libre y usa `java.net.http.HttpClient` contra el servidor verdadero. Es más valioso que simular cada método privado del handler.

## Qué acaba de pasar

HelpDesk ya parece una aplicación backend real: proceso, puerto, JSON, códigos HTTP y tests de integración. Aun así, las reglas siguen ejecutables sin servidor.

## Errores comunes

- Poner toda la lógica dentro del handler.
- Deserializar JSON directamente a un objeto mutable que también representa dominio.
- Escribir JSON concatenando strings.
- Fijar un puerto de test y provocar colisiones intermitentes.

## Buenas prácticas

Mantén DTOs de frontera pequeños, respuestas JSON consistentes y pruebas offline. Una dependencia como Jackson debe resolver una necesidad concreta; Maven debe dejar su versión explícita y reproducible.

## Tu turno

[PAUSA PARA EJERCICIO] Crea un ticket sin propiedad `priority` y comprueba que la respuesta devuelve `NORMAL`. Luego envía `"priority":"URGENTE"` y observa qué status devuelve la frontera.

## Cómo comprobar

```bash
mvn -Dtest=HelpDeskHttpServerTest test
```

## Solución enlazada

La suite canónica contiene ejemplos de creación, listado y errores. Intenta primero leer la respuesta HTTP por ti mismo.

## Reto adicional

Explica qué piezas cambiarían si mañana el adaptador HTTP se reemplazara por Spring MVC. ¿Qué debería permanecer intacto?

## Resumen

- HTTP es una frontera, no el dominio.
- Jackson resuelve JSON; no decide reglas de tickets.
- Un test con socket real protege el contrato de integración.
- Maven hace reproducibles las dependencias.

## Siguiente paso

Continúa con [Lección 04 — Errores explícitos y Checkpoint 01](04-errores-y-checkpoint.md).

## Referencias

- [`HttpServer`](https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/com/sun/net/httpserver/HttpServer.html)
- [`HttpClient`](https://docs.oracle.com/en/java/javase/25/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Jackson project](https://github.com/FasterXML/jackson)

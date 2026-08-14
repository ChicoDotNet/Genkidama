# Lección 04 — Errores explícitos y Checkpoint 01

## Qué vas a conseguir

Vas a distinguir errores de entrada, recursos ausentes y conflictos de estado; después resolverás el primer checkpoint sin copiar la solución.

## Antes de empezar

Completa la [Lección 03](03-api-http-y-json.md) y ejecuta toda la suite.

## El problema

Una API que siempre devuelve `500` obliga a clientes y operadores a adivinar. Un título vacío no es lo mismo que un ticket inexistente ni que intentar avanzar un ticket ya resuelto.

## Concepto

HelpDesk usa excepciones del dominio y las traduce en la frontera:

- `IllegalArgumentException` / JSON inválido → **400 Bad Request**;
- `TicketNotFoundException` → **404 Not Found**;
- `InvalidTicketTransitionException` → **409 Conflict**;
- método no soportado → **405 Method Not Allowed**.

El dominio no contiene números HTTP. La frontera decide cómo representar un fallo para ese protocolo.

## Demostración

[DEMO] Crea un ticket, avánzalo dos veces y luego intenta una tercera:

```bash
curl -X POST http://localhost:8080/api/tickets/1/advance
```

La tercera petición debe ser conflicto, no éxito silencioso.

Prueba también:

```bash
curl -X POST http://localhost:8080/api/tickets/999/advance
```

Debe distinguir ticket ausente.

## Código real

`HelpDeskHttpServer` captura errores concretos:

```java
} catch (TicketNotFoundException exception) {
    send(exchange, 404, new ErrorResponse(exception.getMessage()));
} catch (InvalidTicketTransitionException exception) {
    send(exchange, 409, new ErrorResponse(exception.getMessage()));
}
```

Este mapeo es pequeño y explícito. No inventamos una jerarquía compleja antes de necesitarla.

## Qué acaba de pasar

Ahora el contrato de HelpDesk expresa tanto el camino feliz como fallos normales. Eso mejora pruebas, diagnóstico y capacidad de integración.

## Errores comunes

- Atrapar `Exception` y convertir todo en 400.
- Devolver stack traces internos al cliente.
- Usar `null` para representar “no encontrado”.
- Cambiar una prueba para aceptar el comportamiento incorrecto.
- Confundir validación del protocolo con regla de negocio.

## Buenas prácticas

Los mensajes deben ser útiles pero no revelar secretos o detalles internos. Prueba los fallos esperables. Mantén el dominio independiente del transporte y documenta APIs públicas con Javadoc.

## Tu turno — Checkpoint 01

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) sin abrir la solución.

El checkpoint añade una prioridad crítica **con una regla**, no sólo otro literal del enum.

## Cómo comprobar

```bash
mvn verify
```

Después prueba manualmente un ticket crítico válido e inválido por HTTP.

## Solución enlazada

Sólo después de tu intento consulta [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

## Reto adicional

Diseña, sin implementar, una respuesta JSON con código de error estable además del mensaje humano. ¿Qué ventaja tendría para clientes automatizados?

## Resumen

- 400, 404 y 409 expresan problemas diferentes.
- El dominio lanza errores de negocio; HTTP los traduce.
- Los caminos negativos forman parte del contrato profesional.
- El checkpoint integra tipos, reglas, tests y API.

## Siguiente paso

Continúa con [Lección 05 — Consultas tipadas sin esconder el dominio](05-consultas-tipadas.md).

## Referencias

- [HTTP Semantics — RFC 9110](https://www.rfc-editor.org/rfc/rfc9110)
- [Java Exceptions tutorial](https://dev.java/learn/exceptions/)
- [JUnit assertions](https://docs.junit.org/6.1.2/writing-tests/assertions.html)

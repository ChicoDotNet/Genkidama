# Lección 09 — Resumen operativo con streams

## Qué vas a conseguir

Vas a convertir el estado actual de HelpDesk en un resumen tipado y útil para operación sin duplicar ni mutar tickets.

## Antes de empezar

Completa la [Lección 08](08-persistencia-segura-y-checkpoint.md) y confirma `mvn verify`.

## El problema

Listar tickets sirve para trabajar caso por caso, pero un operador también necesita responder rápido: ¿cuántos están abiertos?, ¿cuántos ya están en curso?, ¿cuántos son de prioridad alta? Recalcularlo en el cliente duplicaría reglas y haría más difícil mantener varios consumidores.

## Concepto

Java permite transformar colecciones con streams sin convertir cada consulta en estado persistido. `TicketSummary` es un `record` inmutable con conteos derivados. `TicketService.summary()` toma un snapshot y calcula cada grupo sin modificar el mapa ni escribir al store.

## Demostración

[DEMO] Crea tres tickets con prioridades distintas y avanza uno. Ejecuta:

```bash
curl http://localhost:8080/api/tickets/summary
```

La respuesta refleja `total`, estados y prioridades actuales.

## Código real

La idea importante es separar **dato persistido** de **vista derivada**:

```java
return tickets.stream()
        .filter(ticket -> ticket.status() == status)
        .count();
```

El resumen puede reconstruirse en cualquier momento desde los tickets; persistirlo generaría dos fuentes de verdad.

## Qué acaba de pasar

HelpDesk ganó un contrato de lectura útil sin ampliar su modelo persistente. Jackson serializa el record en la frontera HTTP, pero el dominio no conoce JSON.

## Errores comunes

- Guardar contadores junto a cada mutación y olvidar actualizarlos en algún camino.
- Calcular estadísticas en el controlador HTTP.
- Mutar la colección mientras se resume.
- Usar strings para estados que ya están modelados como enums.

## Buenas prácticas

Prefiere datos derivados cuando pueden reconstruirse de forma barata y determinista. Usa tipos cerrados y prueba ejemplos representativos antes de optimizar.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una comprobación que demuestre que el resumen cambia después de resolver un ticket, pero que el ticket original sigue accesible por su ID.

## Cómo comprobar

```bash
mvn verify
```

Después crea tickets por HTTP y consulta `/api/tickets/summary`.

## Solución enlazada

La suite del proyecto muestra el comportamiento esperado; intenta primero tu propia aserción.

## Reto adicional

Diseña un resumen por combinación estado+prioridad. Explica cuándo sería más legible un `EnumMap` que varios campos escalares.

## Resumen

- Los resúmenes son vistas derivadas, no otra fuente de verdad.
- Streams expresan conteos sin mutación.
- El dominio conserva tipos; HTTP sólo serializa el resultado.

## Siguiente paso

En la [Lección 10](10-concurrencia-y-executor.md) harás explícito qué ocurre cuando varias peticiones llegan al mismo tiempo.

## Referencias

- [Stream — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/stream/Stream.html)
- [Records — Java Language Guide](https://docs.oracle.com/en/java/javase/25/language/records.html)

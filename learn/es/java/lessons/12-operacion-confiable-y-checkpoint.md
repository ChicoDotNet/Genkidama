# Lección 12 — Operación confiable + Checkpoint 03

## Qué vas a conseguir

Vas a integrar resúmenes derivados, concurrencia y diagnóstico mínimo en una operación coherente de HelpDesk y cerrarás el tercer checkpoint.

## Antes de empezar

Completa la [Lección 11](11-diagnostico-opt-in-sin-pii.md) y conserva `mvn verify` verde.

## El problema

Una API que “funciona” en el camino feliz todavía puede ser frágil si pierde IDs bajo concurrencia, duplica fuentes de verdad o necesita registrar contenido sensible para entender si está fallando.

## Concepto

Este bloque protege tres contratos distintos:

1. **estado de negocio:** `TicketService` mantiene tickets e invariantes;
2. **ejecución concurrente:** el servidor limita workers y el servicio serializa mutaciones locales;
3. **señal operativa:** los diagnósticos agregados observan resultados, no contenido.

Ningún contrato pretende resolver coordinación distribuida o observabilidad de producción completa.

## Demostración

[DEMO] Ejecuta la suite y localiza estas pruebas:

- `computesSummaryFromTheCurrentSnapshot`;
- `concurrentCreatorsReceiveUniqueIdsWithoutLosingTickets`;
- `diagnosticsAreOptInAndExposeOnlyAggregateCounters`.

Cada una protege una frontera distinta.

## Código real

La aplicación activa diagnóstico desde configuración externa:

```java
boolean diagnostics = "1".equals(System.getenv("HELPDESK_DIAGNOSTICS"))
        || "true".equalsIgnoreCase(System.getenv("HELPDESK_DIAGNOSTICS"));
```

El dominio no lee variables de entorno. La configuración permanece en el entry point.

## Qué acaba de pasar

HelpDesk ya no es sólo CRUD didáctico: puede explicar cómo deriva información, cómo se comporta ante peticiones simultáneas y qué señal mínima entrega a operación sin capturar PII.

## Errores comunes

- Llamar “thread-safe” a todo el sistema porque una clase usa `synchronized`.
- Exponer diagnóstico por defecto sin necesidad.
- Confundir un resumen de negocio con una métrica técnica.
- Optimizar locks o streams sin medición.
- Añadir un framework sólo para obtener capacidades ya comprendidas y resueltas por el JDK.

## Buenas prácticas

Documenta el alcance de cada garantía. Mantén configuración e I/O en bordes. Prueba invariantes en el nivel más pequeño posible y conserva una prueba HTTP para verificar integración real.

## Tu turno — Checkpoint 03

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

## Cómo comprobar

```bash
mvn verify
```

Luego inicia HelpDesk con `HELPDESK_DIAGNOSTICS=1`, crea varios tickets y contrasta `/api/tickets/summary` con `/api/diagnostics`.

## Solución enlazada

Consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md) sólo después de tu intento.

## Reto adicional

Propón una estrategia para mover persistencia a una base de datos sin cambiar `Ticket` ni las reglas de transición. Identifica qué garantía de concurrencia debería migrar al almacenamiento.

## Resumen

- Los resúmenes se derivan del estado confirmado.
- El servidor usa concurrencia acotada.
- Las mutaciones locales conservan invariantes.
- El diagnóstico es agregado, opt-in y sin PII.
- Las limitaciones multi-proceso permanecen explícitas.

## Siguiente paso

El próximo bloque 13–16 tratará tooling profesional, debugging, medición/rendimiento y hardening antes de la evaluación final.

## Referencias

- [HttpServer — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/com/sun/net/httpserver/HttpServer.html)
- [ExecutorService — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/ExecutorService.html)
- [LongAdder — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/atomic/LongAdder.html)

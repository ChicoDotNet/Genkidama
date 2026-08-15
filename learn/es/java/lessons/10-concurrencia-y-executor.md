# Lección 10 — Concurrencia explícita y un estado consistente

## Qué vas a conseguir

Vas a entender por qué un servidor HTTP concurrente obliga a proteger el estado compartido y comprobarás que HelpDesk no pierde tickets ni repite IDs bajo carga concurrente local.

## Antes de empezar

Completa la [Lección 09](09-resumen-operativo-y-streams.md).

## El problema

Un servidor real no puede asumir que cada petición termina antes de que llegue la siguiente. Dos creaciones simultáneas podrían leer el mismo `nextId`, escribir snapshots incompatibles o pisarse entre sí si el servicio no define una política de concurrencia.

## Concepto

`HttpServer` ahora usa un `ExecutorService` de tamaño acotado. Esa decisión hace visible la concurrencia en la frontera HTTP. `TicketService`, en cambio, serializa sus operaciones públicas mutables con `synchronized`.

Esto no convierte el archivo JSON en una base de datos multi-proceso. Garantiza únicamente consistencia **dentro de una instancia de HelpDesk**.

## Demostración

[DEMO] La prueba `concurrentCreatorsReceiveUniqueIdsWithoutLosingTickets` lanza veinte creaciones desde cuatro workers. Verifica dos invariantes:

- los veinte IDs son distintos;
- los veinte tickets quedan visibles.

## Código real

La frontera HTTP administra los threads:

```java
executor = Executors.newFixedThreadPool(4);
server.setExecutor(executor);
```

El dominio no crea threads. Sólo protege la sección crítica donde lectura, persistencia y publicación del snapshot deben comportarse como una unidad local.

## Qué acaba de pasar

La arquitectura distingue concurrencia de transporte y consistencia de negocio. No introdujimos locks sofisticados porque `synchronized` ya expresa adecuadamente el contrato actual.

## Errores comunes

- Añadir threads sin identificar estado compartido.
- Suponer que una colección no lanza errores significa que es thread-safe.
- Mantener un lock mientras se hace trabajo externo ilimitado sin reconocer el costo.
- Confundir thread safety dentro del proceso con coordinación entre procesos.

## Buenas prácticas

Empieza por invariantes observables. Usa la sincronización más simple que mantenga corrección y mide antes de optimizar. Cierra explícitamente executors propios para no dejar threads vivos al terminar pruebas o aplicación.

## Tu turno

[PAUSA PARA EJERCICIO] Aumenta temporalmente la prueba concurrente y razona qué parte puede convertirse en cuello de botella si `TicketStore.save()` fuese lento.

## Cómo comprobar

```bash
mvn verify
```

La prueba debe ser determinista: no uses `sleep` para “dar tiempo” a los threads.

## Solución enlazada

La implementación canónica usa futures y espera su resultado; compara después de tu intento.

## Reto adicional

Explica cómo cambiaría el diseño si dos procesos distintos escribieran el mismo archivo JSON. No intentes resolver coordinación distribuida con otro `synchronized`.

## Resumen

- El HTTP concurrente hace visible el problema de estado compartido.
- El executor pertenece a la frontera del servidor.
- `TicketService` protege invariantes locales con sincronización explícita.
- Multi-thread y multi-process son problemas diferentes.

## Siguiente paso

En la [Lección 11](11-diagnostico-opt-in-sin-pii.md) añadirás señal operativa sin registrar datos de los tickets.

## Referencias

- [ExecutorService — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/ExecutorService.html)
- [Executors — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/Executors.html)
- [Intrinsic Locks and Synchronization](https://docs.oracle.com/javase/tutorial/essential/concurrency/locksync.html)

# Lección 08 — Persistir antes de publicar + Checkpoint 02

## Qué vas a conseguir

Vas a evitar estado fantasma cuando falla el almacenamiento y cerrarás el segundo checkpoint con consultas, edición y persistencia real.

## Antes de empezar

Completa la [Lección 07](07-persistencia-json.md) y conserva `mvn verify` verde.

## El problema

Un orden peligroso sería: modificar el mapa en memoria, intentar guardar y descubrir después que el disco falló. El cliente recibiría error, pero una petición posterior vería un cambio que nunca quedó persistido.

## Concepto

HelpDesk construye un snapshot candidato, ejecuta `store.save(...)` y sólo después reemplaza el estado visible. La persistencia se convierte en parte del contrato de la mutación.

`JsonFileTicketStore` escribe primero un archivo temporal y luego lo mueve al destino. Si el filesystem soporta movimiento atómico lo usa; si no, realiza reemplazo explícito. El dominio no necesita conocer ese detalle.

## Demostración

[DEMO] La prueba `failedPersistenceDoesNotExposeCandidateOrConsumeId` usa un `TicketStore` que siempre falla. Después del error, `service.list()` sigue vacío.

Ese test no simula un disco real: verifica la regla más importante sin depender de un sistema externo.

## Código real

El orden está concentrado en una operación:

```java
store.save(List.copyOf(candidate.values()));
tickets.clear();
tickets.putAll(candidate);
```

Si `save` lanza `TicketPersistenceException`, las últimas dos líneas no se ejecutan.

## Qué acaba de pasar

El comportamiento observable coincide con la persistencia confirmada. No prometemos transacciones distribuidas; sí evitamos una inconsistencia local fácil de introducir.

## Errores comunes

- Mutar memoria antes de guardar.
- Atrapar el error de persistencia y continuar como éxito.
- Convertir corrupción en “estado vacío”.
- Afirmar atomicidad universal porque un filesystem soportó `ATOMIC_MOVE` en una máquina.

## Buenas prácticas

Distingue claramente qué garantiza tu diseño. Prueba fallos en la frontera y mantén el camino de error tan determinista como el camino feliz.

## Tu turno — Checkpoint 02

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución.

## Cómo comprobar

```bash
mvn verify
```

Después ejecuta HelpDesk con un archivo temporal, crea tickets, cambia una prioridad, reinicia el proceso y confirma que el estado permanece.

## Solución enlazada

Sólo después de tu intento consulta [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

## Reto adicional

Diseña una estrategia para dos procesos escribiendo el mismo archivo. No la implementes todavía: identifica por qué el snapshot JSON actual no es suficiente para concurrencia multi-proceso.

## Resumen

- Persistir antes de publicar evita estado fantasma.
- El almacenamiento falla de forma explícita.
- JSON corrupto no se silencia.
- El checkpoint integra filtros, edición, I/O y regresiones.

## Siguiente paso

Continúa con la [Lección 09 — Resumen operativo con streams](09-resumen-operativo-y-streams.md), donde separarás datos persistidos de vistas derivadas.

## Referencias

- [StandardCopyOption — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/file/StandardCopyOption.html)
- [AtomicMoveNotSupportedException](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/file/AtomicMoveNotSupportedException.html)
- [JUnit exception assertions](https://docs.junit.org/6.1.2/writing-tests/assertions.html)

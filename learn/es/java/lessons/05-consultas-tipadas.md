# Lección 05 — Consultas tipadas sin esconder el dominio

## Qué vas a conseguir

Vas a consultar tickets por estado y prioridad sin convertir filtros externos en `String` dentro del dominio.

## Antes de empezar

Completa la [Lección 04](04-errores-y-checkpoint.md) y ejecuta `mvn verify`.

## El problema

Con pocos tickets, devolver toda la colección funciona. En una mesa de ayuda real pronto necesitas preguntas como “¿qué tickets HIGH siguen abiertos?” sin duplicar reglas en HTTP.

## Concepto

`TicketQuery` modela filtros opcionales con los mismos tipos del dominio: `TicketStatus` y `TicketPriority`. `null` significa “sin filtro” en esa dimensión. HTTP sigue recibiendo texto, pero lo convierte en enums antes de llamar al servicio.

## Demostración

[DEMO] Crea tickets con distintas prioridades y consulta:

```bash
curl 'http://localhost:8080/api/tickets?status=open&priority=high'
```

Un valor desconocido como `status=waiting` debe devolver 400 en vez de convertirse silenciosamente en “todos”.

## Código real

La regla de coincidencia vive fuera del adapter:

```java
public boolean matches(Ticket ticket) {
    return (status == null || ticket.status() == status)
            && (priority == null || ticket.priority() == priority);
}
```

`TicketService.list(query)` conserva el orden de creación y devuelve un snapshot, no la colección mutable interna.

## Qué acaba de pasar

La API ganó búsquedas útiles sin introducir una dependencia de HTTP en el dominio. La frontera traduce texto; el núcleo trabaja con tipos válidos.

## Errores comunes

- Pasar query strings crudos al dominio.
- Aceptar valores desconocidos como si no hubiera filtro.
- Devolver la colección mutable interna.
- Filtrar alterando el orden sin documentarlo.

## Buenas prácticas

Usa tipos cerrados cuando el conjunto de valores lo sea. Mantén las consultas deterministas y prueba combinaciones representativas.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una consulta que muestre sólo tickets `RESOLVED`. Escribe primero una prueba del servicio y luego comprueba el endpoint.

## Cómo comprobar

```bash
mvn verify
curl 'http://localhost:8080/api/tickets?status=resolved'
```

## Solución enlazada

La solución del bloque se integra en el [Checkpoint 02](../exercises/checkpoint-02.md); intenta primero esta modificación.

## Reto adicional

¿Qué cambiaría si además quisieras buscar por texto? Decide si esa regla pertenece a `TicketQuery`, a un índice de persistencia o a otra frontera y justifica el costo.

## Resumen

- HTTP convierte texto externo a enums.
- `TicketQuery` expresa filtros sin conocer el transporte.
- El resultado mantiene orden y no expone mutabilidad interna.

## Siguiente paso

Continúa con [Lección 06 — Modificar prioridad sin romper el ciclo de vida](06-modificar-prioridad.md).

## Referencias

- [Java enum](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/Enum.html)
- [Stream API](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/stream/Stream.html)

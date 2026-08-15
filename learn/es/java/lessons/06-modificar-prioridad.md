# Lección 06 — Modificar prioridad sin romper el ciclo de vida

## Qué vas a conseguir

Vas a editar una propiedad de negocio conservando identidad y estado, y expondrás la operación con un contrato HTTP específico.

## Antes de empezar

Completa la [Lección 05](05-consultas-tipadas.md).

## El problema

La prioridad de un ticket puede cambiar después de crearlo. Reutilizar `create()` o reconstruir el objeto desde HTTP arriesga perder su estado actual.

## Concepto

`Ticket` es inmutable. Modificarlo significa construir un nuevo valor con el mismo `id`, título, descripción y estado, cambiando sólo la prioridad. `TicketService` sigue siendo la autoridad de esa operación.

## Demostración

[DEMO] Cambia la prioridad del ticket 1:

```bash
curl -X PUT http://localhost:8080/api/tickets/1/priority \
  -H 'Content-Type: application/json' \
  -d '{"priority":"HIGH"}'
```

Después avanza el ticket y confirma que conserva la nueva prioridad.

## Código real

La mutación es explícita:

```java
Ticket updated = new Ticket(
        current.id(),
        current.title(),
        current.description(),
        priority,
        current.status());
```

No existe un setter oculto ni un mapa genérico de campos. El endpoint `/api/tickets/{id}/priority` traduce una intención concreta.

## Qué acaba de pasar

La inmutabilidad no impide cambios de negocio: hace visible qué cambia y qué permanece igual. Esa claridad simplifica pruebas y persistencia.

## Errores comunes

- Hacer mutable todo `Ticket` sólo para editar un campo.
- Permitir prioridad `null`.
- Cambiar el estado accidentalmente durante una edición.
- Añadir un endpoint “patch cualquier cosa” antes de tener una necesidad real.

## Buenas prácticas

Prefiere operaciones con intención de negocio y valida en el límite correcto. Prueba que los campos no relacionados permanecen iguales.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una prueba que cambie la prioridad de un ticket ya `IN_PROGRESS` y demuestre que el estado no regresa a `OPEN`.

## Cómo comprobar

```bash
mvn verify
```

## Solución enlazada

Compara tu resultado con el [Checkpoint 02](../exercises/checkpoint-02.md) sólo después de intentarlo.

## Reto adicional

Diseña cómo representarías una edición de título. ¿Usarías otra operación explícita, un comando compuesto o JSON Patch? No implementes hasta poder justificar el contrato.

## Resumen

- `Ticket` permanece inmutable.
- El servicio reemplaza valores completos de forma controlada.
- La edición conserva identidad y estado.

## Siguiente paso

Continúa con [Lección 07 — Persistencia detrás de una frontera](07-persistencia-json.md).

## Referencias

- [Records — Java language updates](https://docs.oracle.com/en/java/javase/25/language/records.html)
- [HttpRequest.BodyPublisher](https://docs.oracle.com/en/java/javase/25/docs/api/java.net.http/java/net/http/HttpRequest.BodyPublisher.html)

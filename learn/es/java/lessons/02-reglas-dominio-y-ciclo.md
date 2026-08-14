# Lección 02 — Reglas de dominio, colecciones y ciclo de vida

## Qué vas a conseguir

Vas a entender cómo `TicketService` concentra reglas, usa una colección con orden estable y convierte una secuencia de estados en comportamiento probado.

## Antes de empezar

Completa la [Lección 01](01-tu-primer-ticket.md) y ejecuta:

```bash
mvn test
```

## El problema

Representar un ticket no basta. La aplicación debe asignar IDs, normalizar texto y evitar que un ticket resuelto vuelva a avanzar. Si esas reglas se reparten entre HTTP, UI y persistencia, cada frontera puede comportarse distinto.

## Concepto

`TicketService` es una clase de aplicación/dominio pequeña. Mantiene un `LinkedHashMap<Long, Ticket>` porque necesitamos dos propiedades visibles: buscar por ID y conservar orden de creación.

Los métodos públicos son `synchronized` en este primer servidor en memoria. No es una recomendación universal de concurrencia; es una forma pequeña de preservar invariantes mientras varios handlers HTTP podrían acceder al mismo servicio.

La transición se expresa con un `switch` exhaustivo:

```java
TicketStatus nextStatus = switch (current.status()) {
    case OPEN -> TicketStatus.IN_PROGRESS;
    case IN_PROGRESS -> TicketStatus.RESOLVED;
    case RESOLVED -> throw new InvalidTicketTransitionException(current);
};
```

Si en el futuro aparece otro estado, el compilador obliga a revisar este punto.

## Demostración

[DEMO] Recorre `TicketService.create()`. Identifica cuatro decisiones:

1. título obligatorio;
2. espacios externos eliminados;
3. prioridad `NORMAL` si se omite;
4. ID secuencial estable.

Después recorre `advance()` y sigue una misma instancia lógica a través de sus valores inmutables.

## Código real

La prueba del ciclo de vida no inspecciona campos privados:

```java
assertEquals(TicketStatus.IN_PROGRESS, service.advance(created.id()).status());
assertEquals(TicketStatus.RESOLVED, service.advance(created.id()).status());
assertThrows(InvalidTicketTransitionException.class,
        () -> service.advance(created.id()));
```

Eso protege el contrato observable y permite refactorizar internamente sin reescribir la prueba.

## Qué acaba de pasar

Usaste clases, métodos, encapsulación, colecciones, excepciones y `switch` para resolver reglas reales de HelpDesk. El código sigue sin conocer JSON o sockets.

## Errores comunes

- Exponer el `Map` mutable directamente.
- Devolver `null` para ticket inexistente y obligar a cada caller a adivinar qué significa.
- Permitir cualquier transición porque “la UI ya lo controla”.
- Escribir una prueba que conoce el campo privado `tickets`.

## Buenas prácticas

Devuelve snapshots inmutables, usa excepciones con significado concreto y mantén reglas deterministas cerca del dominio. No agregues una interfaz por costumbre: crea una frontera cuando exista un segundo mecanismo o una necesidad clara de sustitución.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una prueba que cree tres tickets y demuestre que `list()` conserva el orden de creación. Intenta primero escribir la expectativa antes de cambiar producción.

## Cómo comprobar

```bash
mvn -Dtest=TicketServiceTest test
```

## Solución enlazada

Compara con las pruebas canónicas sólo después de intentar el ejercicio.

## Reto adicional

¿Qué problema aparecería si varios procesos distintos necesitaran compartir estos tickets? Describe por qué `synchronized` no resolvería ese escenario.

## Resumen

- La colección elegida responde a necesidades observables.
- Las reglas viven fuera de HTTP.
- Las excepciones hacen explícitos fallos esperables.
- Una máquina de estados pequeña puede expresarse sin framework.

## Siguiente paso

Continúa con [Lección 03 — Una API HTTP real con JSON](03-api-http-y-json.md).

## Referencias

- [`LinkedHashMap`](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/LinkedHashMap.html)
- [Switch expressions](https://docs.oracle.com/javase/specs/jls/se25/html/jls-15.html#jls-15.28)
- [`List.copyOf`](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/List.html#copyOf(java.util.Collection))

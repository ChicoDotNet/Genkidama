# Lección 01 — Tu primer ticket: records, enums y JUnit

## Qué vas a conseguir

Vas a compilar Java 25, ejecutar una prueba real y entender los dos tipos que sostienen el modelo inicial de HelpDesk: `Ticket` y sus `enum` de prioridad/estado.

## Antes de empezar

Desde `app/` comprueba:

```bash
java --version
mvn --version
mvn test
```

La línea del JDK debe ser 25. No necesitas IDE para completar el curso.

## El problema

Una mesa de ayuda necesita representar un ticket sin depender todavía de una base de datos, una pantalla o un framework. Si usamos cadenas libres para todo, valores como `abierto`, `OPEN` y `open` pueden terminar significando lo mismo sin que el compilador nos ayude.

## Concepto

Java permite modelar conjuntos cerrados con `enum` y datos inmutables con `record`.

En HelpDesk:

```java
public enum TicketStatus {
    OPEN,
    IN_PROGRESS,
    RESOLVED
}
```

Y un ticket es un valor compuesto:

```java
public record Ticket(
        long id,
        String title,
        String description,
        TicketPriority priority,
        TicketStatus status) {
}
```

Un `record` genera constructor, accesores, `equals`, `hashCode` y `toString` adecuados para datos. Eso no significa que toda clase deba ser record: úsalo cuando la identidad del objeto está expresada principalmente por sus datos.

## Demostración

[EN PANTALLA] Abre `domain/Ticket.java`, `TicketPriority.java` y `TicketStatus.java`. Observa que el dominio no importa HTTP, Jackson ni clases del sistema de archivos.

[EJECUTAR]

```bash
mvn -Dtest=TicketServiceTest test
```

JUnit prueba el comportamiento desde código, no mirando manualmente la consola.

## Código real

`TicketServiceTest` crea un ticket y verifica identificador, normalización, prioridad por defecto y estado inicial. Aunque todavía no estudiaste el servicio completo, ya estás leyendo una especificación ejecutable.

```java
Ticket ticket = service.create(
        "  No puedo iniciar sesión  ",
        "  Error al autenticar  ",
        null);

assertEquals("No puedo iniciar sesión", ticket.title());
assertEquals(TicketStatus.OPEN, ticket.status());
```

## Qué acaba de pasar

El compilador conoce los estados permitidos y JUnit comprueba una regla observable. Ya tienes una pieza de aplicación real, no un ejemplo aislado de `Hello World`.

## Errores comunes

- Cambiar `enum` por `String` porque parece más rápido.
- Usar campos mutables públicos para representar el ticket.
- Memorizar sintaxis sin ejecutar la prueba.
- Confundir `record` con persistencia: un record no guarda nada por sí mismo.

## Buenas prácticas

Mantén nombres de código en inglés coherente, documentación pedagógica en español y tipos cerrados para conceptos cerrados. Deja I/O fuera del dominio hasta que exista una razón concreta.

## Tu turno

[PAUSA PARA EJERCICIO] Agrega temporalmente una prioridad `CRITICAL` al enum y observa qué partes del proyecto compilan sin tocar nada más. Después revierte el cambio: el checkpoint pedirá una regla real, no sólo otro valor.

## Cómo comprobar

```bash
mvn test
```

Debe terminar sin fallos.

## Solución enlazada

Esta lección no tiene solución separada: el código canónico ya es la demostración. Los checkpoints sí separan intento y referencia.

## Reto adicional

Explica en voz alta cuándo preferirías una clase normal sobre un `record`.

## Resumen

- `enum` reduce estados inválidos.
- `record` expresa datos inmutables con poco ruido.
- JUnit convierte expectativas en evidencia ejecutable.
- El dominio todavía no necesita HTTP ni persistencia.

## Siguiente paso

Continúa con [Lección 02 — Reglas de dominio, colecciones y ciclo de vida](02-reglas-dominio-y-ciclo.md).

## Referencias

- [Records — Java Language Specification](https://docs.oracle.com/javase/specs/jls/se25/html/jls-8.html#jls-8.10)
- [Enum Types — Java Language Specification](https://docs.oracle.com/javase/specs/jls/se25/html/jls-8.html#jls-8.9)
- [JUnit User Guide](https://docs.junit.org/6.1.2/)

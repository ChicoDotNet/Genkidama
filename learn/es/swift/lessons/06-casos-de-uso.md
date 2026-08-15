# Lección 06 — Casos de uso sin framework

## Qué vas a conseguir

Moverás las operaciones de aplicación a `TimeQuoteService` sin introducir SwiftUI, red ni base de datos.

## El problema

La CLI ya coordina demasiadas decisiones: crea clientes, registra tiempo y consulta resúmenes. Si mañana aparece una UI, no conviene duplicar ese flujo.

## Concepto

`TimeQuoteService` recibe un repositorio, carga un `TimeQuoteBook` y ofrece operaciones de aplicación. La UI futura podrá pedir "agrega cliente" o "registra tiempo" sin conocer cómo persiste el estado.

## Demostración

[EJECUTAR]

```bash
swift run TimeQuote
swift test
```

Observa que `main.swift` consume el servicio en vez de manipular directamente el libro.

## Tu turno

Agrega una consulta que muestre todos los resúmenes mediante el servicio. No expongas los diccionarios privados de `TimeQuoteBook`.

## Errores comunes

- Convertir el servicio en una clase gigante con todas las reglas.
- Meter presentación o persistencia concreta dentro del caso de uso.
- Duplicar validaciones que ya pertenecen al dominio.

## Buenas prácticas

La frontera de aplicación coordina; el dominio conserva las reglas.

## Resumen

La CLI y una futura UI pueden reutilizar los mismos casos de uso.

## Siguiente paso

Continúa con [la lección 07](07-repositorio-en-memoria.md).

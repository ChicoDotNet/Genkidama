# Lección 12 — Prueba persistencia entre instancias

## Qué vas a conseguir

Probarás la propiedad que justifica todo el bloque: el estado sobrevive cuando desaparecen las instancias en memoria.

## El problema

Una prueba que guarda y lee usando el mismo objeto puede pasar aunque la persistencia real esté rota. Necesitamos demostrar que una nueva instancia puede reconstruir el estado únicamente desde el archivo.

## Concepto

La prueba crea `firstService`, registra un cliente y tiempo, y después crea `secondService` con un `FileTimeQuoteRepository` nuevo apuntando a la misma URL. El segundo servicio no comparte memoria con el primero.

```swift
let secondService = try TimeQuoteService(
    repository: FileTimeQuoteRepository(fileURL: fileURL)
)
let summary = try secondService.summary(for: client.id)
```

## Demostración

[EJECUTAR]

```bash
cd app
swift test
```

La prueba debe verificar minutos e importe, no sólo que el archivo exista.

## Tu turno

Amplía temporalmente la prueba con una segunda entrada de tiempo y predice el total antes de ejecutarla.

## Cómo comprobar

La prueba sólo es convincente si las dos instancias del servicio/repository son distintas y el dato esperado se reconstruye desde disco.

## Errores comunes

- Comprobar únicamente `fileExists`.
- Reutilizar accidentalmente el mismo repositorio en memoria.
- Escribir una prueba dependiente de una ruta global que deja residuos.

## Buenas prácticas

Usa directorios temporales únicos y elimínalos al final. Prueba comportamiento observable, no detalles del JSON.

## Resumen

TimeQuote ya tiene una frontera sustituible y evidencia de persistencia durable real.

## Checkpoint

Realiza [Checkpoint 03](checkpoint-03.md).

## Siguiente paso

El siguiente bloque introducirá concurrencia y estado de aplicación sólo cuando exista una necesidad visible para mantener TimeQuote responsivo y preparado para una UI SwiftUI.

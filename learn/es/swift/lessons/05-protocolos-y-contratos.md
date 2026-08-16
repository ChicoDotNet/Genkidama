# Lección 05 — Protocolos como contratos reemplazables

## Qué vas a conseguir

Separarás lo que TimeQuote necesita de almacenamiento de la forma concreta en que se almacena.

## El problema

Si el dominio conoce archivos, SQLite o CloudKit, cambiar infraestructura obliga a reescribir reglas de negocio.

## Concepto

Swift usa `protocol` para describir capacidades. `TimeQuoteRepository` sólo promete `load()` y `save(_:)`; no decide todavía si la información vive en memoria o en disco.

```swift
public protocol TimeQuoteRepository {
    mutating func load() throws -> TimeQuoteBook
    mutating func save(_ book: TimeQuoteBook) throws
}
```

## Demostración

Revisa [`Repository.swift`](../app/Sources/TimeQuote/Repository.swift) y ejecuta:

```bash
swift test
```

## Tu turno

Explica por escrito qué tendría que cambiar para sustituir `InMemoryTimeQuoteRepository` por almacenamiento en archivo. La respuesta correcta no debe requerir modificar `Client` ni `TimeEntry`.

## Errores comunes

- Crear protocolos sin una necesidad de sustitución real.
- Copiar todos los métodos de una clase concreta al protocolo.
- Hacer que el dominio importe detalles del almacenamiento.

## Buenas prácticas

Un protocolo pequeño expresa una necesidad del consumidor, no el catálogo completo del proveedor.

## Resumen

TimeQuote ya tiene una frontera explícita entre reglas y almacenamiento.

## Siguiente paso

Continúa con [la lección 06](06-casos-de-uso.md).

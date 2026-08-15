# Lección 09 — Serializa el estado sin filtrar infraestructura

## Qué vas a conseguir

Harás que `TimeQuoteBook` pueda representarse como JSON sin cambiar las reglas de negocio ni el contrato `TimeQuoteRepository`.

## El problema

Hasta ahora el repositorio en memoria pierde todo al terminar el proceso. Antes de guardar en disco necesitamos una representación durable, pero no queremos introducir diccionarios sueltos o JSON dentro de `TimeQuoteService`.

## Concepto

Swift sintetiza `Codable` cuando todos los stored properties también son codificables. En TimeQuote añadimos `Codable` a `Client`, `TimeEntry`, `ClientSummary` y `TimeQuoteBook`. Eso permite que la infraestructura use `JSONEncoder`/`JSONDecoder` mientras el dominio sigue hablando en tipos propios.

[EN PANTALLA]

```swift
public struct Client: Codable, Equatable, Sendable { ... }
public struct TimeQuoteBook: Codable, Sendable { ... }
```

## Demostración

[EJECUTAR]

```bash
cd app
swift test
```

Las pruebas existentes deben seguir pasando: hacer un tipo serializable no debe cambiar su semántica.

## Tu turno

Explica por qué sería peor devolver `Data` desde `TimeQuoteService` que hacer `TimeQuoteBook` codificable y dejar la conversión al repositorio.

## Cómo comprobar

El código de `TimeQuoteService` no debe importar `Foundation` ni mencionar JSON, archivos o rutas.

## Errores comunes

- Convertir cada operación de negocio directamente a JSON.
- Usar diccionarios `[String: Any]` y perder seguridad de tipos.
- Confundir `Codable` con una garantía de compatibilidad eterna del formato.

## Buenas prácticas

Mantén la serialización en la frontera. Si el formato cambia en una versión futura, la migración debe resolverse cerca de persistencia.

## Resumen

TimeQuote ya puede transformarse a una representación durable sin ensuciar casos de uso.

## Siguiente paso

Continúa con [la lección 10](10-repositorio-json-durable.md) para guardar esa representación en disco.

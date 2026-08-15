# Lección 04 — Errores explícitos y pruebas de comportamiento

## Qué vas a conseguir

Harás que TimeQuote falle de manera comprensible y protegerás reglas con Swift Testing.

## El problema

Un ID duplicado o una entrada de tiempo para un cliente inexistente no deben convertirse en corrupción silenciosa.

## Concepto

`TimeQuoteError` enumera fallos del dominio. Las operaciones que pueden producirlos usan `throws`. El caller decide si puede recuperarse, mostrar un mensaje o terminar la operación.

Las pruebas viven en [`TimeQuoteBookTests.swift`](../app/Tests/TimeQuoteTests/TimeQuoteBookTests.swift) y usan Swift Testing:

```swift
@Test func rejectsTimeForUnknownClient() throws
```

`#expect` comprueba comportamiento, no nombres de clases ni detalles internos.

## Demostración

[EJECUTAR]

```bash
swift test
```

Deberías observar las pruebas verdes.

## Tu turno

Escribe una prueba que intente crear un cliente con tarifa `0` y espere `TimeQuoteError.invalidHourlyRate`.

Después cambia la tarifa a un valor válido y conserva la prueba como defensa de regresión.

## Cómo comprobar tu solución

```bash
swift test
```

Todas las pruebas deben pasar sin comentar validaciones ni reemplazar errores por valores mágicos.

## Errores comunes

- `try?` usado sólo para ocultar un error que debería manejarse.
- Capturar `Error` y no hacer nada.
- Probar únicamente la salida feliz.
- Escribir tests que verifican la estructura privada en vez del contrato observable.

## Buenas prácticas

Un error útil tiene semántica suficiente para tomar una decisión. Una prueba útil protege una regla que podría romperse durante mantenimiento.

## Resumen

El primer slice de TimeQuote ya tiene entradas válidas, fallos explícitos y pruebas ejecutables.

## Checkpoint

Realiza [Checkpoint 01](checkpoint-01.md) antes de avanzar al siguiente bloque.

## Siguiente paso

Después del checkpoint, continúa con [la lección 05](05-protocolos-y-contratos.md).

## Referencias

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/errorhandling
- https://developer.apple.com/xcode/swift-testing/

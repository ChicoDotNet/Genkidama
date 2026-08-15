# Lección 11 — Fallos de I/O explícitos

## Qué vas a conseguir

Distinguirás entre un error de negocio y un fallo de persistencia sin esconder ninguno de los dos.

## El problema

Un archivo puede no poder leerse, contener JSON inválido o fallar al escribirse. Si dejamos escapar errores arbitrarios de Foundation, el consumidor no sabe qué contrato ofrece nuestra infraestructura.

## Concepto

TimeQuote introduce `PersistenceError` con casos separados para lectura, datos inválidos, codificación y escritura. El repositorio traduce errores técnicos a ese contrato, mientras `TimeQuoteError` sigue reservado para reglas del negocio.

```swift
public enum PersistenceError: Error, Equatable, Sendable {
    case readFailed
    case invalidData
    case encodingFailed
    case writeFailed
}
```

## Demostración

La prueba `fileRepositoryRejectsCorruptDataExplicitly` escribe contenido inválido y exige `PersistenceError.invalidData`.

[EJECUTAR]

```bash
cd app
swift test
```

## Tu turno

Provoca deliberadamente un archivo JSON corrupto y explica por qué no debería convertirse en `TimeQuoteError.clientNotFound` ni en un libro vacío.

## Cómo comprobar

Los datos corruptos deben fallar de forma visible; sólo la ausencia inicial del archivo representa un estado vacío válido.

## Errores comunes

- Capturar cualquier error y devolver datos vacíos.
- Convertir un fallo técnico en un error de negocio.
- Mostrar detalles internos del filesystem como parte del contrato del dominio.

## Buenas prácticas

Haz explícito qué fallos puede observar el consumidor y conserva suficiente información para diagnosticar sin cambiar el significado del dominio.

## Resumen

TimeQuote diferencia reglas inválidas de almacenamiento defectuoso.

## Siguiente paso

Continúa con [la lección 12](12-prueba-persistencia-entre-instancias.md).

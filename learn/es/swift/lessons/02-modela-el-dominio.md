# Lección 02 — Modela clientes y tiempo con tipos de Swift

## Qué vas a conseguir

Entenderás `struct`, propiedades, inicializadores, optionals y value semantics mientras haces que TimeQuote represente datos válidos.

## El problema

Guardar todo como cadenas o números sueltos permite estados absurdos: clientes sin nombre, tarifas negativas o registros de cero minutos.

## Concepto

Swift favorece tipos que expresan intención. `Client` y `TimeEntry` son `struct`: valores que se copian con semántica de valor y que pueden validar sus invariantes al crearse.

Revisa [`Domain.swift`](../app/Sources/TimeQuote/Domain.swift).

`note: String?` es optional: una nota puede existir o no. Eso es distinto de inventar una cadena vacía para significar ausencia.

## Demostración

[EN PANTALLA]

Busca el inicializador de `Client`:

```swift
public init(id: String, name: String, hourlyRateCents: Int) throws
```

No devuelve un cliente inválido. Si los datos no cumplen el contrato, lanza un error explícito.

## Tu turno

Intenta crear temporalmente un cliente con nombre en blanco. Ejecuta:

```bash
swift test
```

y observa cómo las pruebas documentan el comportamiento esperado.

Después restaura un nombre válido.

## Qué acaba de pasar

Los tipos reducen estados inválidos antes de que una pantalla o una base de datos multipliquen el problema.

## Errores comunes

- Usar `String` vacío como sustituto de todos los optionals.
- Convertir cada tipo en `class` por costumbre de otros lenguajes.
- Validar sólo en la UI y dejar el dominio sin protección.

## Buenas prácticas

Empieza con `struct` para valores de dominio y elige `class` sólo cuando identidad compartida, herencia u otra semántica de referencia sea realmente necesaria.

## Tu turno

Añade una nota opcional distinta al registro de `main.swift`. Después prueba `note: nil` y confirma que el modelo acepta ambos casos.

## Cómo comprobar

```bash
swift test
swift run TimeQuote
```

## Resumen

Modelaste datos con tipos y optionals en vez de depender de convenciones frágiles.

## Siguiente paso

Continúa con [colecciones y totales](03-colecciones-y-totales.md).

## Referencias

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/classesandstructures
- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/thebasics#Optionals

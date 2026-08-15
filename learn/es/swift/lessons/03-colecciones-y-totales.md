# Lección 03 — Colecciones y cálculo de importes

## Qué vas a conseguir

Usarás diccionarios, arreglos, `filter`, `reduce`, ordenamiento y funciones para convertir registros de tiempo en información útil.

## El problema

Un solo registro no sirve para facturar un mes. TimeQuote necesita acumular clientes y múltiples entradas sin perder la relación entre ellos.

## Concepto

`TimeQuoteBook` usa dos colecciones con responsabilidades distintas:

- `[String: Client]` permite encontrar un cliente por ID;
- `[TimeEntry]` conserva los registros de tiempo.

En [`TimeQuoteBook.swift`](../app/Sources/TimeQuote/TimeQuoteBook.swift), `summary(for:)` filtra entradas del cliente y usa `reduce` para sumar minutos.

## Cálculo monetario

La tarifa vive en **centavos enteros**. Evitamos `Double` para dinero porque los binarios de punto flotante pueden introducir redondeos inesperados.

La fórmula del slice actual es:

```text
importe en centavos = minutos × tarifa por hora / 60
```

Más adelante haremos explícita la política de redondeo cuando aparezcan cotizaciones con reglas más ricas.

## Demostración

Añade un segundo registro para el mismo cliente y ejecuta:

```bash
swift test
swift run TimeQuote
```

Las pruebas ya demuestran que 30 + 90 minutos a $600/h producen 120 minutos y $1,200.

## Tu turno

Agrega un segundo cliente en `main.swift`, registra tiempo para ambos y recorre `book.allSummaries()` para imprimir una línea por cliente.

## Cómo comprobar

Los clientes deben aparecer ordenados por nombre y cada total debe contener sólo sus propias entradas.

## Errores comunes

- Usar `Double` para centavos sin una política de precisión.
- Sumar todos los registros antes de separar por cliente.
- Guardar el nombre del cliente como llave mutable en vez de un identificador estable.

## Buenas prácticas

Elige la colección por el acceso que necesitas, no por costumbre. Un diccionario expresa búsqueda por llave; un arreglo expresa secuencia.

## Resumen

TimeQuote ya transforma múltiples registros en totales por cliente.

## Siguiente paso

Continúa con [errores y pruebas](04-errores-y-pruebas.md).

## Referencias

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/collectiontypes
- https://developer.apple.com/documentation/swift/array/reduce(_:_:)

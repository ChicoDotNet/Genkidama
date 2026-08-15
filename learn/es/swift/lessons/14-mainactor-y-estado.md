# Lección 14 — Aísla estado de aplicación con MainActor

## Qué vas a conseguir

Vas a modelar el estado que una interfaz necesita y a protegerlo con `MainActor` para evitar escrituras concurrentes incoherentes.

## El problema

Una UI necesita distinguir entre “todavía no cargué”, “estoy cargando”, “ya tengo datos” y “algo falló”. Si varias tareas pueden escribir esas variables sin una regla de aislamiento, aparecen carreras y estados imposibles.

## Concepto

`@MainActor` es un actor global. Un tipo aislado allí garantiza que su estado mutable se accede siguiendo ese aislamiento. Eso encaja con el estado que posteriormente consumirá SwiftUI.

TimeQuote usa un enum explícito:

```swift
public enum TimeQuoteViewState: Equatable, Sendable {
    case idle
    case loading
    case loaded([ClientSummary])
    case failed(String)
}
```

No usamos cuatro booleanos como `isLoading`, `hasData`, `hasError` y `isEmpty`, porque podrían producir combinaciones contradictorias.

## Demostración

```swift
@MainActor
public final class TimeQuoteApplication<Repository: TimeQuoteRepository> {
    public private(set) var state: TimeQuoteViewState = .idle
}
```

Las pruebas que interactúan con este tipo declaran también su aislamiento:

```swift
@Test @MainActor
func applicationPublishesLoadedStateAfterAsyncRefresh() async throws {
    // ...
}
```

[EJECUTAR]

```bash
cd app
swift test
```

## Qué acaba de pasar

La aplicación ahora tiene una máquina de estados pequeña y explícita. La UI futura sólo tendrá que renderizar ese estado y enviar intenciones; no decidir reglas de negocio.

## Errores comunes

- Usar `DispatchQueue.main.async` por costumbre en código moderno sin entender aislamiento.
- Guardar el mismo estado mutable en la vista y en el servicio.
- Representar errores sólo con `print`.
- Añadir `@MainActor` al dominio entero.

## Buenas prácticas

Aísla el estado que realmente pertenece a presentación/aplicación. Mantén `Client`, `TimeEntry`, `TimeQuoteBook` y sus reglas independientes de la UI.

## Tu turno

Añade en papel un caso `empty` y decide si de verdad agrega información que `loaded([])` no expresa ya. Justifica tu decisión.

## Cómo comprobar

Una buena decisión evita estados redundantes y hace más simple el renderizado.

## Reto adicional

Enumera qué acciones de usuario necesitará una pantalla mínima: recargar, agregar cliente y registrar tiempo. Relaciónalas con métodos de `TimeQuoteApplication`.

## Resumen

`MainActor` protege la frontera mutable de aplicación; `TimeQuoteViewState` hace visibles estados que la UI necesita representar.

## Siguiente paso

Continúa con [la lección 15 — Convierte errores en estado visible](15-errores-como-estado.md).

## Referencias

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/

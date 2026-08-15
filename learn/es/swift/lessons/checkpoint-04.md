# Checkpoint 04 — Estado responsivo y frontera de UI

## Objetivo

Demostrar que puedes representar carga, éxito y error sin mover reglas de negocio a una futura vista SwiftUI.

## Misión

1. Construye `TimeQuoteApplication` con un repositorio en memoria.
2. Agrega un cliente y registra tiempo usando métodos asíncronos de la frontera de aplicación.
3. Ejecuta `refresh()` y comprueba que el estado termina en `.loaded` con los minutos e importe esperados.
4. Intenta registrar tiempo para un cliente inexistente y comprueba que el estado termina en `.failed`.
5. Explica por qué `TimeQuoteBook` y `TimeQuoteService` no necesitan `@MainActor`.
6. Dibuja cómo una futura `DashboardView` renderizaría cada caso de `TimeQuoteViewState`.

## Restricciones

- No introduzcas `sleep` para fabricar asincronía.
- No conviertas todo el dominio en `async`.
- No leas/escribas archivos directamente desde la frontera visual.
- No declares una implementación SwiftUI compilada si sólo estás diseñando la frontera.

## Cómo comprobar

[EJECUTAR]

```bash
cd app
swift build
swift test
swift run TimeQuote
```

El checkpoint está terminado cuando las pruebas demuestran al menos un estado de éxito y un failure mode visible, y el núcleo SwiftPM sigue compilando sin depender de SwiftUI.

## Reflexión

¿Qué ganarías y qué perderías si `TimeQuoteViewState` se reemplazara por varios booleanos independientes?

## Solución

Revisa:

- [`ApplicationState.swift`](../app/Sources/TimeQuote/ApplicationState.swift)
- [`TimeQuoteBookTests.swift`](../app/Tests/TimeQuoteTests/TimeQuoteBookTests.swift)

## Siguiente paso

Continúa con [la evaluación final de TimeQuote](17-evaluacion-final.md). Tendrás que modificar la base sin receta y justificar tanto decisiones de lenguaje como fronteras de arquitectura.

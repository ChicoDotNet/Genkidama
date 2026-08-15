# Lección 16 — Prepara una frontera para SwiftUI

## Qué vas a conseguir

Vas a conectar lo aprendido con la arquitectura de una pantalla SwiftUI sin obligar a que el curso deje de ser ejecutable y testeable en Linux.

## El problema

SwiftUI sólo está disponible en plataformas Apple con Xcode, mientras que el núcleo del curso se valida de forma reproducible con SwiftPM en Linux. Si mezclamos `View`, dominio y persistencia en los mismos archivos, perdemos portabilidad, aislamiento y pruebas rápidas.

## Concepto

La solución no es evitar SwiftUI: es definir una frontera clara.

Una vista futura debería hacer dos cosas:

1. renderizar `TimeQuoteViewState`;
2. enviar intenciones a `TimeQuoteApplication`.

Pseudocódigo de la futura pantalla:

```swift
struct DashboardView: View {
    let application: TimeQuoteApplication<FileTimeQuoteRepository>

    var body: some View {
        switch application.state {
        case .idle, .loading:
            ProgressView()
        case .loaded(let summaries):
            SummaryList(summaries: summaries)
        case .failed(let message):
            ErrorView(message: message)
        }
    }
}
```

No copies este pseudocódigo dentro del paquete Linux: el objetivo es reconocer la frontera, no fingir que SwiftUI está disponible donde no lo está.

## Demostración

La parte que sí podemos probar hoy es el contrato de estado:

```swift
await application.refresh()
#expect(application.state == .loaded([expected]))
```

Eso reduce la cantidad de lógica que necesitará una prueba de UI futura.

[EJECUTAR]

```bash
cd app
swift build
swift test
swift run TimeQuote
```

## Qué acaba de pasar

La arquitectura queda preparada para añadir un target Apple sin mover reglas al framework. SwiftUI será un consumidor del estado, no la fuente de verdad del negocio.

## Errores comunes

- Poner cálculos de importes dentro de `View.body`.
- Leer JSON directamente desde una vista.
- Duplicar `TimeQuoteBook` en estado visual.
- Afirmar que una UI fue compilada cuando sólo existe pseudocódigo.

## Buenas prácticas

Distingue con honestidad evidencia portable de evidencia específica de plataforma. Cuando llegue la etapa SwiftUI final, valida el target con macOS/Xcode; mientras tanto, mantén el núcleo verde en Linux.

## Tu turno

Dibuja el flujo:

`SwiftUI View → TimeQuoteApplication → TimeQuoteService → TimeQuoteRepository → almacenamiento`

Anota qué responsabilidad pertenece a cada frontera.

## Cómo comprobar

Ninguna flecha debe permitir que SwiftUI escriba JSON ni que el repositorio decida textos visuales.

## Reto adicional

Propón dónde colocarías navegación y formatting monetario sin contaminar el dominio.

## Resumen

TimeQuote ya tiene una frontera asíncrona y de estado adecuada para convertirse después en una app SwiftUI, conservando un núcleo SwiftPM portable y testeable.

## Checkpoint

Completa [Checkpoint 04 — Estado responsivo y frontera de UI](checkpoint-04.md).

## Siguiente paso

Después del checkpoint sólo queda la evaluación final: extender TimeQuote sin receta paso a paso y demostrar que puedes explicar las decisiones tomadas.

## Referencias

- https://developer.apple.com/documentation/swiftui
- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/

# Solución de referencia — Evaluación final de TimeQuote

Esta solución no es la única válida. Úsala para comparar fronteras, pruebas y trade-offs después de completar tu propio intento.

## 1. Estados de cotización

Una opción pequeña es introducir un `enum QuoteStatus: String, Codable` con `draft`, `sent`, `accepted` y `rejected`, y encapsular las transiciones permitidas en el tipo que representa la cotización o en un método del dominio.

La prueba importante no es comprobar que existen cuatro casos. Comprueba una regla observable, por ejemplo que una cotización `accepted` no pueda regresar silenciosamente a `draft`.

## 2. Persistencia corrupta

`FileTimeQuoteRepository` debe conservar la distinción entre:

- archivo inexistente: todavía no hay estado persistido;
- JSON válido con colecciones vacías: estado vacío legítimo;
- JSON presente pero ilegible/incompatible: `PersistenceError`.

Una prueba de regresión puede escribir bytes JSON truncados en un archivo temporal, intentar cargar el repositorio y verificar que se propaga un error de persistencia.

## 3. Resumen por cliente

Si el resumen se calcula exclusivamente a partir de entidades ya cargadas, una opción razonable es mantener el cálculo cerca del dominio (`TimeQuoteBook`) y hacer que `TimeQuoteService` lo exponga como caso de uso.

Evita añadir una consulta especializada al repositorio mientras el volumen y la tecnología de almacenamiento actuales no lo necesiten. Si después el estado creciera hasta requerir agregación en almacenamiento, ese nuevo dato justificaría mover la optimización detrás de una frontera de lectura.

## 4. Trabajo asíncrono

Mantén cálculos de minutos/importes síncronos. Usa `async` en la frontera de aplicación cuando existe suspensión o coordinación externa real/esperable.

`TimeQuoteApplication.refresh()` ya ofrece el lugar correcto para suspender sin contaminar entidades. Si introduces una abstracción futura de sincronización, haz que esa frontera sea asíncrona; no conviertas `Client` o `TimeEntry` en tipos asíncronos.

## 5. Estado visible durante refresco

En lugar de varios booleanos independientes, una evolución posible de `TimeQuoteViewState` es distinguir:

- `idle`;
- `loading`;
- `loaded([ClientSummary])`;
- `refreshing([ClientSummary])`;
- `failed(String)`.

Así una futura vista puede conservar datos durante un refresco y mostrar progreso sin inventar combinaciones imposibles como `isLoading == true` e `isFailed == true` simultáneamente.

## 6. Sincronización futura

No conviertas `TimeQuoteRepository` en una interfaz que haga persistencia, HTTP, autenticación, conflictos y telemetría.

Una frontera pequeña podría comenzar como:

```swift
protocol TimeQuoteSyncing {
    func synchronize() async throws
}
```

Si después aparecen necesidades reales —por ejemplo cursors, conflictos o batches— evoluciona ese contrato con evidencia. Mientras no haya backend real, no añadas una pila HTTP sólo para demostrar arquitectura.

## Pruebas mínimas valiosas

Una solución razonable debería proteger al menos:

- una transición inválida de cotización;
- corrupción de persistencia;
- cálculo del resumen por cliente;
- transición de estado durante refresco o un failure mode asíncrono.

No añadas tests cuyo único objetivo sea inflar coverage.

## Frontera SwiftUI

El núcleo portable puede demostrar en Linux:

- reglas de dominio;
- persistencia JSON;
- casos de uso;
- concurrencia Swift;
- estado observable de aplicación.

Una aplicación SwiftUI real requiere crear un target Apple y validarlo con macOS/Xcode. La arquitectura actual permite hacerlo sin mover reglas al framework, pero no debemos presentar ese target como compilado mientras esa evidencia no exista.

## Una decisión deliberadamente no implementada

Un ejemplo defendible es **no introducir sincronización HTTP real** todavía. No existe un backend en el alcance de TimeQuote, por lo que añadir cliente HTTP, autenticación y mocks aumentaría superficie sin resolver un requisito presente. El contrato mínimo puede esperar a que aparezca evidencia del servicio remoto.

## Comprobación final

```bash
cd app
swift build
swift test
swift run TimeQuote
```

Después compara tu solución contra la rúbrica de la [evaluación final](../lessons/17-evaluacion-final.md). Una alternativa distinta es válida si conserva comportamiento, pruebas y separación de responsabilidades y puedes defender sus trade-offs.

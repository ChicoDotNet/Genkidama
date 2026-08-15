# 14 — Modela estado observable para la UI

## Qué vas a conseguir
Preparar una UI Android que observe estado explícito sin convertir la pantalla en dueña de las reglas de negocio.

## Antes de empezar
Completa la lección 13. Debes distinguir dominio, caso de uso, repositorio y adaptador de almacenamiento.

## El problema
Una pantalla necesita representar carga, datos y errores. Si sólo ejecuta llamadas y muta widgets, pronto mezcla navegación, persistencia, reglas y presentación.

## Concepto
Modela el estado que la UI puede renderizar. En Kotlin un `sealed interface` vuelve explícitos los estados válidos y permite `when` exhaustivo.

```kotlin
sealed interface WorkOrdersUiState {
    data object Loading : WorkOrdersUiState
    data class Ready(val orders: List<WorkOrder>) : WorkOrdersUiState
    data class Error(val message: String) : WorkOrdersUiState
}
```

En Android moderno, un `ViewModel` puede exponer este estado mediante `StateFlow`. La UI observa; el ViewModel coordina; `WorkOrderService` conserva las reglas.

## Demostración
[DEMO] Traza el flujo `tap -> ViewModel -> WorkOrderService -> Repository -> nuevo UiState -> recomposición`. Marca dónde puede aparecer un error y quién lo transforma en información presentable.

## Código real
`WorkOrderResult` ya te enseñó a no ocultar resultados relevantes. El mismo principio se aplica a presentación: evita estados imposibles como `loading=true`, `error!=null` y una lista simultáneamente considerada válida.

## Qué acaba de pasar
La UI obtiene un contrato pequeño y observable. El dominio sigue siendo utilizable desde CLI, tests JVM u otra interfaz.

## Errores comunes
- guardar `Activity`, `Context` o vistas dentro del dominio;
- exponer colecciones mutables;
- usar excepciones como único mecanismo de estado de pantalla;
- duplicar validaciones del servicio en el ViewModel.

## Buenas prácticas
Haz inmutable el estado publicado y transforma resultados del caso de uso en estados de presentación en una frontera clara.

## Tu turno
Diseña el estado de una pantalla que permita listar órdenes, mostrar error y marcar una orden como completada. Explica qué datos NO deberían vivir ahí.

## Cómo comprobar
Puedes probar la transición de estados sin renderizar una pantalla completa y sin cambiar `WorkOrder`.

## Solución enlazada
Revisa [`WorkOrderResult.kt`](../app/src/main/kotlin/dev/genkidama/fieldflow/WorkOrderResult.kt) para reutilizar la idea de estados explícitos y exhaustivos.

## Reto adicional
¿Cómo evitarías que una rotación o recreación de pantalla repita una operación destructiva?

## Resumen
El estado observable hace que la UI sea una proyección del sistema, no el lugar donde vive el sistema.

## Siguiente paso
Continúa con [15 — Construye una pantalla Compose](15-compose.md).

## Referencias
- https://developer.android.com/topic/architecture/ui-layer
- https://developer.android.com/kotlin/flow/stateflow-and-sharedflow

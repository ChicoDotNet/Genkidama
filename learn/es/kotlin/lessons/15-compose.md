# 15 — Construye una pantalla Compose desde el estado

## Qué vas a conseguir
Convertir `WorkOrdersUiState` en una interfaz Android declarativa sin trasladar reglas de negocio a composables.

## Antes de empezar
Completa la lección 14 y entiende por qué el estado publicado es inmutable.

## El problema
Compose facilita dibujar una UI rápidamente, pero también facilita meter consultas, mutaciones y decisiones de negocio directamente en funciones de presentación.

## Concepto
Un composable debería recibir estado y emitir intenciones del usuario. La coordinación queda fuera de la función visual.

```kotlin
@Composable
fun WorkOrdersScreen(
    state: WorkOrdersUiState,
    onComplete: (String) -> Unit,
) {
    when (state) {
        WorkOrdersUiState.Loading -> CircularProgressIndicator()
        is WorkOrdersUiState.Error -> Text(state.message)
        is WorkOrdersUiState.Ready -> LazyColumn {
            items(state.orders, key = { it.id }) { order ->
                WorkOrderRow(order = order, onComplete = { onComplete(order.id) })
            }
        }
    }
}
```

## Demostración
[EN PANTALLA] Renderiza mentalmente los tres estados. Observa que `WorkOrdersScreen` no sabe si los datos vienen de Room, archivo o memoria.

## Código real
La interfaz `WorkOrderRepository` y `WorkOrderService` siguen siendo la frontera funcional. Compose consume una representación preparada para UI y envía eventos hacia el coordinador.

## Qué acaba de pasar
La dependencia apunta desde la plataforma hacia el núcleo. Cambiar la pantalla no obliga a cambiar persistencia; cambiar persistencia no obliga a reescribir la pantalla.

## Errores comunes
- llamar al DAO desde un composable;
- crear repositorios durante cada recomposición;
- usar índices de lista como identidad estable;
- esconder `Loading` y `Error` detrás de una lista vacía.

## Buenas prácticas
Haz composables pequeños, recibe datos inmutables, eleva eventos y usa claves estables en listas.

## Tu turno
Diseña `WorkOrderRow` con título, prioridad, estado y acción para completar. Después describe qué debería ocurrir si `onComplete` devuelve un error.

## Cómo comprobar
La pantalla puede previsualizarse o probarse con estados fabricados sin inicializar Room ni ejecutar una operación de red.

## Solución enlazada
Usa [`WorkOrder.kt`](../app/src/main/kotlin/dev/genkidama/fieldflow/WorkOrder.kt) para identificar los datos mínimos que necesita cada fila.

## Reto adicional
Añade conceptualmente un filtro por prioridad sin hacer que el composable modifique el repositorio directamente.

## Resumen
Compose renderiza estado y comunica intenciones; no reemplaza las fronteras que ya construiste.

## Siguiente paso
Continúa con [16 — Offline first y sincronización](16-offline-first.md).

## Referencias
- https://developer.android.com/develop/ui/compose
- https://developer.android.com/develop/ui/compose/state
- https://developer.android.com/develop/ui/compose/lists

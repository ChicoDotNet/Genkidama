# 13 — Implementa Room como adaptador Android

## Qué vas a conseguir
Traducir la frontera de persistencia que ya conoces a Room sin mover reglas de negocio al framework.

## Antes de empezar
Completa la lección 12 y el Checkpoint 03. Debes poder explicar por qué `WorkOrderRepository` no menciona Android, SQL ni Room.

## El problema
FieldFlow necesita una base local Android consultable y durable. Si hacemos que `WorkOrder` sea directamente una entidad Room, las decisiones de almacenamiento empiezan a gobernar el dominio.

## Concepto
Room pertenece al borde de datos. El módulo Android puede definir `WorkOrderEntity`, `WorkOrderDao` y `RoomWorkOrderRepository`; el núcleo conserva `WorkOrder`, `WorkOrderResult`, `WorkOrderService` y `WorkOrderRepository`.

La relación es deliberada:

```text
UI Android -> WorkOrderService -> WorkOrderRepository <- RoomWorkOrderRepository -> WorkOrderDao -> Room
```

## Demostración
[EN PANTALLA] Compara `WorkOrderRecord` con una futura `WorkOrderEntity`. Ambas representan almacenamiento; ninguna necesita convertirse en el modelo de negocio.

Una entidad mínima podría conservar `id`, `title`, `priority` y `status` como columnas. El adaptador convierte explícitamente entidad ↔ dominio y satisface el contrato existente.

## Código real
La rama mantiene ejecutable el núcleo JVM y su `FileWorkOrderRepository`. Ésa es la referencia de comportamiento que el adaptador Room debe preservar: `findAll`, `findById` y `save` no cambian para `WorkOrderService`.

## Qué acaba de pasar
Introdujiste una tecnología Android sin reescribir el caso de uso. Ésa es la prueba práctica de que la frontera de la lección 7 tenía valor.

## Errores comunes
- anotar `WorkOrder` con `@Entity` sólo para ahorrar un mapper;
- devolver tipos Room desde `WorkOrderRepository`;
- esconder errores de base de datos como listas vacías;
- mover todas las pruebas a instrumentación Android.

## Buenas prácticas
Conserva en JVM las pruebas de dominio y casos de uso. Añade pruebas Room sólo para SQL, mapping y comportamiento que dependa realmente de Room.

## Tu turno
Diseña `WorkOrderEntity`, las firmas de `WorkOrderDao` y el mapper entidad ↔ dominio. Luego enumera qué pruebas existentes deberían seguir pasando sin modificación.

## Cómo comprobar
Tu diseño es correcto si `WorkOrderService` puede recibir un `RoomWorkOrderRepository` sin importar `android.*` ni `androidx.room.*`.

## Solución enlazada
Usa [`WorkOrderRepository.kt`](../app/src/main/kotlin/dev/genkidama/fieldflow/WorkOrderRepository.kt) como contrato y [`WorkOrderRecord.kt`](../app/src/main/kotlin/dev/genkidama/fieldflow/WorkOrderRecord.kt) como ejemplo de representación persistida separada.

## Reto adicional
¿Dónde colocarías una migración de esquema cuando agregues `assignedTechnician`? Explica por qué no pertenece a `WorkOrderService`.

## Resumen
Room es un adaptador de persistencia Android, no el centro del modelo de FieldFlow.

## Siguiente paso
Continúa con [14 — Estado observable para una UI Android](14-estado-ui.md).

## Referencias
- https://developer.android.com/training/data-storage/room
- https://developer.android.com/topic/architecture/data-layer

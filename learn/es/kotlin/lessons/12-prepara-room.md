# 12 — Diseña el salto a Room sin romper el núcleo

## Qué vas a conseguir
Entender exactamente qué debe cambiar al entrar a Android/Room y qué contratos deben permanecer estables.

## Antes de empezar
Completa las lecciones 9–11 y revisa `WorkOrderRepository`, `WorkOrderRecord` y `FileWorkOrderRepository`.

## El problema
Ahora que la persistencia funciona, es tentador introducir Room en todas partes. Eso convertiría una decisión de almacenamiento Android en una dependencia del dominio y de los casos de uso.

## Concepto
La siguiente implementación Android tendrá tres piezas equivalentes a las que ya conoces: una entidad de almacenamiento, un mapeo entidad ↔ dominio y un repositorio que implementa `WorkOrderRepository`. `WorkOrderService` seguirá hablando únicamente con el contrato.

## Demostración
[PAUSA PARA EJERCICIO] Dibuja dos columnas: “permanece” y “cambia”. Coloca `WorkOrder`, `WorkOrderResult`, `WorkOrderService` y `WorkOrderRepository` en la primera; coloca JSON, `Path` y `FileWorkOrderRepository` en la segunda.

## Código real
`WorkOrderRecord` demuestra que una representación persistida puede variar sin modificar `WorkOrder`. Room repetirá esa idea con una entidad y un DAO en lugar de un archivo JSON.

## Qué acaba de pasar
El framework entra porque ahora existe una necesidad concreta: consultas y persistencia local Android más robustas. No entra para adornar la arquitectura.

## Errores comunes
- pasar entidades Room directamente a la UI y al dominio;
- importar anotaciones Android en reglas de negocio;
- reemplazar pruebas rápidas JVM por instrumentación para todo;
- adoptar Room antes de entender el contrato que implementa.

## Buenas prácticas
Mantén las pruebas del núcleo en JVM. Añade pruebas Android sólo donde haya comportamiento que realmente dependa de Android o Room.

## Tu turno
Escribe la firma que tendría un futuro `RoomWorkOrderRepository` sin implementarlo. Debe satisfacer `WorkOrderRepository` y no requerir cambios en `WorkOrderService`.

## Cómo comprobar
Tu diseño debe permitir sustituir `InMemoryWorkOrderRepository` o `FileWorkOrderRepository` por la implementación Room desde el composition root.

## Solución enlazada
Revisa [`WorkOrderRepository.kt`](../app/src/main/kotlin/dev/genkidama/fieldflow/WorkOrderRepository.kt) y confirma que el contrato no menciona Android, JSON, archivos ni SQL.

## Reto adicional
¿Qué operaciones deberían convertirse en consultas del DAO si FieldFlow crece a miles de órdenes?

## Resumen
Ya tienes una razón, una frontera y criterios para introducir Room sin reescribir el núcleo.

## Siguiente paso
Primero realiza [Checkpoint 03 — Persistencia offline](checkpoint-03.md) y después continúa con [13 — Implementa Room como adaptador Android](13-room-adapter.md).

## Referencias
- https://developer.android.com/training/data-storage/room
- https://developer.android.com/topic/architecture/data-layer

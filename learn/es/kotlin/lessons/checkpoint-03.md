# Checkpoint 03 — Persistencia offline

Demuestra que puedes extender la durabilidad de FieldFlow sin acoplar dominio e infraestructura.

## Misión
1. Añade una segunda orden y persiste ambas con `FileWorkOrderRepository`.
2. Recrea el repositorio y demuestra que las dos siguen disponibles.
3. Completa una orden, persístela y verifica que `DONE` sobrevive otra reapertura.
4. Agrega una prueba para un fallo de almacenamiento que no deba convertirse silenciosamente en `emptyList()`.
5. Escribe un bosquejo de `RoomWorkOrderRepository` que implemente el mismo contrato sin modificar `WorkOrderService`.

## Criterios de aceptación
- `gradle test` queda verde;
- no escribes datos de prueba dentro del repositorio Git;
- el dominio no recibe anotaciones de JSON, Room ni Android;
- no capturas `Exception` para ocultar corrupción;
- explicas qué parte será sustituida por Room y qué parte permanecerá estable.

## Reflexión
Si mañana la aplicación necesitara sincronizar con un servidor, ¿qué contrato actual reutilizarías y qué nueva frontera necesitarías?

## Siguiente paso
El siguiente bloque incorporará Android/Room de forma deliberada, conservando las pruebas rápidas del núcleo Kotlin/JVM.

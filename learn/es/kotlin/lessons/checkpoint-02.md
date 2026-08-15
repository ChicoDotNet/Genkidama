# Checkpoint 02 — Fronteras y resultados

Sin mirar una solución paso a paso, demuestra que entiendes el slice 5–8.

## Misión
1. Añade al servicio una operación `find(id)` que devuelva `Success` o `NotFound`.
2. Escribe al menos dos pruebas de comportamiento para esa operación.
3. Crea una función que traduzca `WorkOrderResult<WorkOrder>` a un mensaje de UI sin lanzar excepciones.
4. Explica en cinco líneas por qué `WorkOrderRepository` no debe depender todavía de Room.

## Criterios de aceptación
- el proyecto compila;
- `gradle test` queda verde;
- no introduces Android ni una dependencia nueva;
- no capturas `Exception` para decidir flujo normal;
- las pruebas validan resultados observables.

## Reflexión
¿Qué cambiará cuando el repositorio en memoria sea sustituido por Room y qué debería permanecer idéntico?

## Siguiente paso
En el próximo bloque entraremos a persistencia durable y luego a la plataforma Android con una razón visible para cada dependencia.

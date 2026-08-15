# 10 — Persistencia durable detrás del repositorio

## Qué vas a conseguir
Guardar órdenes en disco y volver a leerlas después de recrear el repositorio.

## Antes de empezar
Completa la lección 9 y conserva `WorkOrderRepository` como contrato de aplicación.

## El problema
`InMemoryWorkOrderRepository` desaparece al terminar el proceso. Para una app offline necesitamos que el estado sobreviva reinicios sin obligar a `WorkOrderService` a conocer archivos o JSON.

## Concepto
`FileWorkOrderRepository` implementa el mismo contrato que el repositorio en memoria. Carga una colección, reemplaza por `id` y persiste un snapshot JSON.

## Demostración
[EJECUTAR] `gradle test` y observa `saved orders survive repository recreation`.

## Código real
La prueba crea un directorio temporal, guarda `WO-100`, construye una segunda instancia del repositorio y recupera la misma orden. El caso de uso no cambia.

## Qué acaba de pasar
La durabilidad apareció como detalle de infraestructura detrás de una frontera que ya existía. Esa es la razón práctica para haber separado responsabilidades antes.

## Errores comunes
- hacer que el servicio abra archivos directamente;
- asumir que `save` sólo inserta y terminar duplicando una misma orden;
- escribir tests contra una ruta real del equipo del alumno.

## Buenas prácticas
Prueba persistencia en directorios temporales y verifica comportamiento después de recrear el componente. Así demuestras durabilidad, no sólo que una función devolvió éxito.

## Tu turno
Agrega una prueba que guarde dos órdenes distintas, reabra el repositorio y compruebe que ambas siguen disponibles.

## Cómo comprobar
`gradle test` debe quedar verde sin crear archivos permanentes dentro del repositorio Git.

## Solución enlazada
Usa [`FileWorkOrderRepositoryTest.kt`](../app/src/test/kotlin/dev/genkidama/fieldflow/FileWorkOrderRepositoryTest.kt) como referencia después de intentar tu prueba.

## Reto adicional
¿Qué necesitaría cambiar si dos procesos escribieran el mismo archivo simultáneamente?

## Resumen
FieldFlow ya puede conservar trabajo sin red ni servidor.

## Siguiente paso
Continúa con [11 — Escrituras seguras y fallos de almacenamiento](11-escrituras-seguras.md).

## Referencias
- https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html

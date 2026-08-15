# 09 — Serializa datos sin contaminar el dominio

## Qué vas a conseguir
Persistir una orden como datos serializables sin convertir `WorkOrder` en un objeto dependiente del formato de almacenamiento.

## Antes de empezar
Debes haber completado las lecciones 1–8 y entender `WorkOrderRepository`.

## El problema
Un objeto de dominio y un registro almacenado cambian por razones distintas. Si anotamos directamente todo el dominio para JSON, el formato de disco empieza a dictar decisiones del negocio.

## Concepto
`WorkOrderRecord` es un modelo de persistencia pequeño. `toRecord()` y `toDomain()` hacen explícita la traducción. Kotlin Serialization se usa sólo en la frontera de almacenamiento.

## Demostración
[EN PANTALLA] Abre `WorkOrderRecord.kt` y compara sus `String` persistidos con los enums tipados de `WorkOrder`.

## Código real
`@Serializable` pertenece al registro; `WorkOrder` permanece sin anotaciones de infraestructura. Los mapeos convierten `Priority` y `WorkOrderStatus` mediante sus nombres estables.

## Qué acaba de pasar
La aplicación conserva un dominio independiente de JSON. Cuando Room sustituya este almacenamiento educativo, la regla de negocio no tendrá que adoptar tipos de Room.

## Errores comunes
- serializar el dominio por comodidad y después acoplarlo a cada detalle del formato;
- guardar `toString()` y asumir que siempre será un contrato estable;
- ignorar qué ocurre cuando un valor persistido deja de existir.

## Buenas prácticas
Haz explícitos los límites entre dominio y almacenamiento. Una conversión de cinco líneas suele ser más barata que esconder acoplamiento durante meses.

## Tu turno
Agrega a `WorkOrderRecord` un campo opcional `notes` con valor por defecto, sin modificar todavía `WorkOrder`. Explica qué decisión faltaría antes de llevarlo al dominio.

## Cómo comprobar
Ejecuta `gradle test`. El proyecto debe compilar y las pruebas existentes deben seguir verdes.

## Solución enlazada
Compara tu enfoque con los mapeos de [`WorkOrderRecord.kt`](../app/src/main/kotlin/dev/genkidama/fieldflow/WorkOrderRecord.kt); no hay una solución única para el campo opcional porque todavía no pertenece al dominio canónico.

## Reto adicional
¿Qué estrategia usarías para migrar un enum renombrado sin romper archivos existentes?

## Resumen
Separaste el contrato persistido del modelo que protege reglas de negocio.

## Siguiente paso
Continúa con [10 — Persistencia durable detrás del repositorio](10-persistencia-durable.md).

## Referencias
- https://kotlinlang.org/docs/serialization.html
- https://kotlinlang.org/docs/serialization-basic-serialization.html

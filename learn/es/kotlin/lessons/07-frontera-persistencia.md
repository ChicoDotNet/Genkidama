# 07 — Una frontera para persistencia

## Qué vas a conseguir
Definir un contrato de almacenamiento que hoy funciona en memoria y después podrá implementarse con Room.

## El problema
FieldFlow necesita ser offline, pero introducir Room antes de dominar la regla convertiría el curso de Kotlin en ceremonia Android.

## Concepto
`WorkOrderRepository` describe las capacidades que el servicio necesita: guardar, buscar y listar. `InMemoryWorkOrderRepository` es una implementación real y pequeña para aprender y probar la frontera.

## Demostración
[EN PANTALLA] Cambia la instancia concreta del repositorio sin modificar `WorkOrderService`.

## Código real
La interfaz no menciona SQLite, Room, Android ni detalles de serialización.

## Qué acaba de pasar
La regla de aplicación ya puede sobrevivir al cambio de almacenamiento.

## Errores comunes
- diseñar una interfaz copiando cada método de la base de datos;
- exponer SQL o tipos de framework al dominio;
- añadir métodos que ningún caso de uso necesita.

## Buenas prácticas
Define la frontera desde necesidades reales del consumidor y hazla crecer con evidencia.

## Tu turno
Implementa temporalmente un repositorio que siempre regrese una sola orden y úsalo en un test.

## Cómo comprobar
El servicio debe compilar y funcionar sin cambios.

## Reto adicional
Escribe qué deberá resolver una futura implementación Room además de `save` y `find`.

## Resumen
Ya existe un punto de sustitución claro entre aplicación y persistencia.

## Siguiente paso
Cerraremos el bloque ejercitando la integración de resultados, servicio y repositorio.

## Referencias
- https://kotlinlang.org/docs/interfaces.html

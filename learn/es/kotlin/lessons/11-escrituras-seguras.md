# 11 — Escrituras seguras y fallos de almacenamiento

## Qué vas a conseguir
Reducir el riesgo de dejar datos incompletos cuando una escritura falla y convertir corrupción en un error explícito.

## Antes de empezar
Completa la persistencia durable de la lección 10.

## El problema
Sobrescribir directamente `orders.json` puede dejar un archivo truncado si el proceso falla durante la escritura. Además, tratar JSON corrupto como una lista vacía ocultaría pérdida de datos.

## Concepto
FieldFlow escribe primero un archivo temporal y después intenta moverlo de forma atómica al destino. Si el sistema de archivos no soporta `ATOMIC_MOVE`, usa un reemplazo normal. Al leer datos inválidos lanza un `IllegalStateException` con contexto de almacenamiento.

## Demostración
[EN PANTALLA] Sigue `persist()` en `FileWorkOrderRepository.kt`: crear temporal → escribir snapshot → mover → limpiar temporal.

## Código real
`try/finally` garantiza la limpieza del temporal. `SerializationException` y valores persistidos inválidos se traducen a errores con la ruta afectada.

## Qué acaba de pasar
No hicimos imposible todo fallo, pero eliminamos una ventana de corrupción evitable y dejamos de confundir “sin datos” con “datos ilegibles”.

## Errores comunes
- capturar cualquier excepción y devolver `emptyList()`;
- dejar archivos temporales abandonados;
- llamar “transacción” a una secuencia sin garantía observable.

## Buenas prácticas
Diseña los fallos como parte del contrato operativo. Los datos corruptos merecen una señal visible; no una recuperación silenciosa que pierda evidencia.

## Tu turno
Escribe un archivo con contenido inválido y verifica que `findAll()` falle con un mensaje que incluya la ruta.

## Cómo comprobar
`gradle test` debe ejecutar el caso `corrupted data fails with storage context` y quedar verde.

## Solución enlazada
Consulta [`FileWorkOrderRepositoryTest.kt`](../app/src/test/kotlin/dev/genkidama/fieldflow/FileWorkOrderRepositoryTest.kt) después de tu intento.

## Reto adicional
Describe qué garantías adicionales ofrece una base de datos transaccional frente a este snapshot de archivo.

## Resumen
La persistencia offline ahora distingue ausencia, durabilidad y corrupción, y reduce escrituras parciales.

## Siguiente paso
Continúa con [12 — Diseña el salto a Room sin romper el núcleo](12-prepara-room.md).

## Referencias
- https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/StandardCopyOption.html

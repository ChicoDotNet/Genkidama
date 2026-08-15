# 05 — Sealed types para resultados explícitos

## Qué vas a conseguir
Representar éxito y fallos esperables sin depender de excepciones para el flujo normal.

## El problema
Una futura pantalla Android necesita distinguir validación, ausencia y éxito para mostrar estados diferentes. Capturar `Exception` borra esa intención.

## Concepto
Una `sealed interface` limita las variantes conocidas y permite `when` exhaustivo. `WorkOrderResult` modela `Success`, `Invalid` y `NotFound` como datos del contrato.

## Demostración
[DEMO] Abre `WorkOrderResult.kt` y observa que cada variante transporta sólo la información que necesita el consumidor.

## Código real
El caso de uso podrá devolver `WorkOrderResult<WorkOrder>` sin obligar a la UI a conocer excepciones internas.

## Qué acaba de pasar
El contrato ahora expresa resultados de negocio de forma tipada.

## Errores comunes
- crear una jerarquía enorme para errores que nunca se distinguen;
- usar excepciones para estados esperables;
- convertir todos los errores técnicos en mensajes de dominio.

## Buenas prácticas
Mantén pocas variantes y nómbralas por la decisión que necesita tomar el consumidor.

## Tu turno
Añade una función local con `when` exhaustivo que convierta cada variante en un mensaje corto.

## Cómo comprobar
El compilador debe obligarte a cubrir todas las variantes.

## Reto adicional
Explica cuándo seguirías prefiriendo una excepción.

## Resumen
Los sealed types hacen explícito el conjunto de resultados esperables.

## Siguiente paso
Continúa con [Casos de uso sin depender de Android](06-casos-de-uso.md).

## Referencias
- https://kotlinlang.org/docs/sealed-classes.html

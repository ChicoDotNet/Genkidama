# Checkpoint 04 — Diagnostica y endurece PocketLedger

## Objetivo
Demostrar que puedes diagnosticar una aplicación sin filtrar datos personales y que sabes verificar una entrega más allá de “en mi máquina funciona”.

## Escenario
Soporte recibe un reporte: PocketLedger carga, pero una persona sospecha que el total visible no coincide con sus categorías. No debes pedirle que copie todas sus descripciones de gastos.

## Tu trabajo
1. Carga al menos tres gastos de dos categorías en un `MemoryExpenseStore`.
2. Obtén `controller.diagnostics()`.
3. Comprueba con una prueba que `expenseCount`, `totalCents` y los conteos por categoría son correctos.
4. Usa descripciones deliberadamente sensibles en los fixtures y demuestra que no aparecen en el diagnóstico.
5. Ejecuta formatter, analyzer y tests.
6. Si tu plataforma lo permite, construye el artefacto web de release.

## Criterios observables
- La prueba falla si un total o conteo es incorrecto.
- La prueba falla si el diagnóstico expone una descripción de fixture.
- No modificas la API para introducir un segundo estado mutable.
- No desactivas ninguna validación para obtener verde.

## Restricción
No agregues logging de objetos `Expense` completos. El objetivo es aprender a reducir datos, no a trasladar el problema.

Cuando termines, compara con la solución de referencia: [`../solutions/checkpoint-04-diagnostico.md`](../solutions/checkpoint-04-diagnostico.md).

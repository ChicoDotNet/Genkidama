# Evaluación final — PocketLedger

Trabaja sobre la aplicación existente. No hay una receta paso a paso: usa el código, las pruebas y la documentación oficial para decidir dónde hacer cada cambio.

## Historias

1. **Presupuesto por categoría:** compara el gasto mensual de una categoría contra un presupuesto en centavos y expón el resultado sin mezclar la regla con widgets.
2. **Bug de fecha:** reproduce un gasto UTC que cambia de mes al interpretarlo localmente, define la semántica temporal de PocketLedger y protege el arreglo con una prueba de regresión.
3. **Persistencia diagnosticable:** mejora el contexto de un fallo de lectura/escritura sin borrar datos corruptos, ocultar la causa o filtrar descripciones.
4. **UI probada:** muestra la señal de presupuesto y añade una prueba de widget para dentro/excedido.
5. **Diagnóstico agregado:** añade una señal relacionada con presupuesto a `ExpenseDiagnostics` sin texto introducido por la persona usuaria.
6. **Diseño futuro:** diseña una frontera mínima para permitir otra implementación de persistencia; no implementes nube, autenticación ni sincronización.

## Condiciones de aceptación

- El dinero sigue representado mediante enteros en centavos.
- Las reglas nuevas son testeables sin montar widgets.
- Un fallo de persistencia no publica un estado que no quedó guardado.
- La solución temporal evita doble conteo y tiene una semántica documentada.
- Los diagnósticos no incluyen descripciones de gastos.
- Hay al menos una prueba de regresión y una prueba de widget nuevas.
- `dart format lib test`, `flutter analyze`, `flutter test` y `flutter build web --release` pasan.
- Incluyes una referencia a documentación oficial consultada y una nota de diseño sobre lo que dejaste fuera.

## Entrega

Escribe una nota breve con: problema encontrado, decisión tomada, evidencia que lo demuestra y un trade-off que aceptarías revisar si PocketLedger creciera.

Sólo después de intentarlo consulta la [solución de referencia](../solutions/final-pocketledger.md).

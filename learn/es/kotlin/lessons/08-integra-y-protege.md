# 08 — Integra el flujo y protege comportamiento

## Qué vas a conseguir
Conectar resultados tipados, casos de uso y repositorio con pruebas que observen comportamiento útil.

## El problema
Separar archivos no garantiza un diseño sano. Necesitamos demostrar que crear persiste, duplicar no corrompe datos, completar maneja ausencias y pendientes mantiene su orden.

## Concepto
Las pruebas de `WorkOrderServiceTest` atraviesan la frontera de aplicación usando una implementación de repositorio real en memoria. No necesitan mocks para probar este slice.

## Demostración
[EJECUTAR] `gradle test` y relaciona cada caso con una regla observable.

## Código real
Los tests verifican persistencia de creación, rechazo de duplicados, `NotFound` y ordenamiento excluyendo completadas.

## Qué acaba de pasar
La futura UI podrá cambiar sin que las reglas dependan de ella, y Room podrá entrar detrás del mismo contrato.

## Errores comunes
- probar sólo que un método fue llamado;
- medir calidad únicamente por porcentaje de coverage;
- hacer assertions sobre detalles internos sin valor para el usuario.

## Buenas prácticas
El piso de coverage del proyecto es 44% cuando sea medible y significativo; prioriza contratos y regresiones reales antes que perseguir 100%.

## Tu turno
Agrega un test para completar una orden existente y verifica que el repositorio conserva `DONE`.

## Cómo comprobar
`gradle test` debe quedar verde y el servicio no debe importar paquetes Android.

## Reto adicional
Describe el estado de UI apropiado para cada variante de `WorkOrderResult`.

## Resumen
FieldFlow ya tiene una frontera de aplicación testeable y preparada para persistencia offline.

## Siguiente paso
Realiza el checkpoint antes de introducir persistencia durable.

## Referencias
- https://kotlinlang.org/api/core/kotlin-test/

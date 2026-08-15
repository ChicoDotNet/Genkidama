# 06 — Casos de uso sin depender de Android

## Qué vas a conseguir
Mover creación, finalización y consulta a un servicio que una futura UI pueda consumir sin conocer almacenamiento ni excepciones.

## El problema
Si una Activity o Composable contiene reglas de duplicados, validación y ordenamiento, esas reglas quedan atadas a Android y son más difíciles de probar.

## Concepto
`WorkOrderService` coordina una intención del usuario y devuelve resultados tipados. No es un patrón agregado por exhibición: es una separación directa de responsabilidades para mantener la regla testeable.

## Demostración
[EJECUTAR] `gradle test`. Revisa `WorkOrderServiceTest.kt` y comprueba que no existe dependencia de Android.

## Código real
`create`, `complete` y `pending` forman la frontera que la interfaz podrá invocar más adelante.

## Qué acaba de pasar
La aplicación ya tiene operaciones orientadas a intención, no sólo un contenedor de objetos.

## Errores comunes
- poner reglas en callbacks de UI;
- devolver `Any` o strings ambiguos;
- crear un servicio gigante con responsabilidades no relacionadas.

## Buenas prácticas
Cada operación debe tener un contrato pequeño y observable.

## Tu turno
Agrega un test para título vacío y verifica `Invalid`.

## Cómo comprobar
`gradle test` debe permanecer verde.

## Reto adicional
Propón una operación `reopen` sin implementarla y define su contrato.

## Resumen
El caso de uso desacopla intención de usuario de detalles de interfaz.

## Siguiente paso
Ahora separaremos también el lugar donde viven las órdenes.

## Referencias
- https://kotlinlang.org/docs/functions.html

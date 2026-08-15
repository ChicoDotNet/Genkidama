# 02 — Modela el dominio con `data class` y `enum`

## Qué vas a conseguir
Entender tipos, propiedades, constructores, `data class`, `enum class` y validación con `require`.

## El problema
Si prioridad y estado fueran strings, `"urgnete"` compilaría y el error aparecería tarde. Necesitamos representar sólo estados válidos.

## Concepto
`data class` genera semántica útil de valor (`equals`, `hashCode`, `copy`, representación). Un `enum class` limita un concepto a alternativas conocidas. El bloque `init` protege invariantes desde la construcción.

## Demostración
[EN PANTALLA] Revisa `WorkOrder.kt`: `Priority`, `WorkOrderStatus` y `WorkOrder` forman el vocabulario mínimo.

## Código real
`complete()` no muta la orden: usa `copy` para devolver una nueva con estado `DONE`.

## Qué acaba de pasar
El compilador ahora ayuda a impedir estados imposibles y la validación rechaza ids/títulos vacíos en el límite del dominio.

## Errores comunes
- usar `data class` para cualquier objeto sin preguntarse si tiene semántica de valor;
- convertir todos los estados en booleanos;
- aceptar texto vacío y tratar de arreglarlo en la UI.

## Buenas prácticas
Haz explícitas las invariantes pequeñas cerca del dato que protegen.

## Tu turno
Añade una propiedad opcional `assignee: String? = null` y crea una orden asignada y otra sin asignar.

## Cómo comprobar
El proyecto debe compilar y ambas órdenes deben poder construirse sin `!!`.

## Reto adicional
Rechaza `assignee` cuando sea un string presente pero en blanco.

## Resumen
Modelaste datos con tipos, nullability y validación en vez de convenciones implícitas.

## Siguiente paso
[Colecciones y reglas de prioridad](03-colecciones-y-reglas.md).

## Referencias
- https://kotlinlang.org/docs/data-classes.html
- https://kotlinlang.org/docs/enum-classes.html
- https://kotlinlang.org/docs/null-safety.html

# 03 — Colecciones y reglas de prioridad

## Qué vas a conseguir
Usar colecciones, lambdas, secuencias y transformaciones para responder una pregunta de negocio.

## El problema
Un técnico no necesita “todos los registros”: necesita primero las órdenes abiertas más urgentes.

## Concepto
Kotlin ofrece operaciones como `filter`, `sortedByDescending`, `map` y secuencias. Se leen como una transformación de datos, pero cada paso debe expresar una regla real.

## Demostración
Revisa `WorkOrderBoard.pendingByPriority()`: filtra completadas y ordena por prioridad antes de materializar una lista.

## Código real
El tablero mantiene un `MutableMap` internamente para localizar ids, pero devuelve listas hacia afuera en vez de exponer la colección mutable.

## Qué acaba de pasar
Encapsulaste mutabilidad local y construiste una consulta legible sin compartir el almacenamiento interno.

## Errores comunes
- encadenar transformaciones sólo por estilo;
- devolver directamente una colección mutable;
- ordenar por texto (`HIGH`, `LOW`) accidentalmente.

## Buenas prácticas
La colección elegida debe corresponder al acceso requerido: aquí un mapa evita búsquedas lineales por id.

## Tu turno
Crea `fun openCount(): Int` y prueba que disminuye al completar una orden.

## Cómo comprobar
`gradle test` debe seguir verde y tu nueva prueba debe observar el conteo antes/después.

## Reto adicional
Implementa una consulta por prioridad sin duplicar la lógica de “sólo abiertas”.

## Resumen
Ya conviertes colecciones en consultas de negocio con funciones y lambdas.

## Siguiente paso
[Errores explícitos y pruebas](04-errores-y-pruebas.md).

## Referencias
- https://kotlinlang.org/docs/collections-overview.html
- https://kotlinlang.org/docs/sequences.html

# 04 — Errores explícitos y pruebas

## Qué vas a conseguir
Proteger comportamiento con pruebas y distinguir una entrada inválida de una entidad inexistente.

## El problema
Duplicar un id o completar `OT-404` no debería fallar silenciosamente. Tampoco queremos pruebas que sólo comprueben nombres de clases.

## Concepto
`require` comunica una precondición del llamador y produce `IllegalArgumentException`. Una búsqueda sin resultado puede expresarse con `NoSuchElementException`. Las pruebas deben observar contratos: orden, transición de estado y rechazo de operaciones inválidas.

## Demostración
[EJECUTAR] `gradle test`. Revisa `WorkOrderBoardTest.kt` y relaciona cada test con una regla visible.

## Código real
Las pruebas usan `kotlin.test`: no necesitamos un framework de mocking para un núcleo pequeño y determinista.

## Qué acaba de pasar
El dominio ya puede evolucionar con una red de seguridad basada en comportamiento.

## Errores comunes
- capturar `Exception` sin distinguir el contrato;
- probar getters triviales para inflar coverage;
- usar mocks cuando una instancia real es más sencilla.

## Buenas prácticas
Busca suficiente cobertura para proteger comportamiento y regresiones; en Genkidama >=44% es piso aprobable cuando es medible y relevante, no una invitación a perseguir 100%.

## Tu turno
Añade el test de `openCount()` de la lección anterior y un caso con dos prioridades iguales.

## Cómo comprobar
`gradle test` debe terminar verde.

## Reto adicional
Discute por escrito si `complete` debería ser idempotente o rechazar una segunda llamada; no cambies el contrato sin una decisión explícita.

## Resumen
Ya validas reglas, errores y cambios con tests pequeños y útiles.

## Siguiente paso
Continúa con [Sealed types para resultados explícitos](05-sealed-results.md) para que la futura UI Android no dependa de excepciones.

## Referencias
- https://kotlinlang.org/api/core/kotlin-stdlib/kotlin/require.html
- https://kotlinlang.org/api/core/kotlin-test/

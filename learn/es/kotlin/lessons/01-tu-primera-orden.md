# 01 — Tu primera orden de trabajo

## Qué vas a conseguir
Ejecutar Kotlin y representar una orden real en FieldFlow.

## Antes de empezar
Necesitas JDK 17 y Gradle. Android Studio llegará después; aquí queremos feedback rápido sobre el lenguaje.

## El problema
Un técnico necesita ver qué trabajo debe atender. Antes de dibujar pantallas necesitamos representar esa información sin texto suelto ni posiciones mágicas.

## Concepto
Kotlin permite declarar valores con `val`, inferir tipos y usar interpolación con `${...}`. La función `main` es nuestro primer punto de entrada.

## Demostración
[DEMO] Ejecuta `gradle run` dentro de `app/`. Verás dos órdenes ordenadas por prioridad.

## Código real
Abre `app/src/main/kotlin/dev/genkidama/fieldflow/Main.kt`. Observa que `WorkOrder` ya expresa id, título y prioridad como datos con nombre.

## Qué acaba de pasar
Compilaste Kotlin a JVM y ejecutaste código real de la misma aplicación que crecerá durante el curso.

## Errores comunes
- confundir `val` con una constante global: significa referencia no reasignable;
- concatenar strings cuando la interpolación comunica mejor la intención;
- instalar Android antes de comprobar que JDK/Gradle funcionan.

## Buenas prácticas
Prefiere nombres de dominio y valores inmutables por defecto.

## Tu turno
Agrega una tercera orden en `Main.kt` y comprueba que aparece en la salida.

## Cómo comprobar
`gradle run` debe terminar sin error y mostrar los tres ids.

## Reto adicional
Haz que la tercera orden sea `HIGH` y predice dónde aparecerá antes de ejecutar.

## Resumen
Ya ejecutas Kotlin, lees una llamada a constructor y modificas una aplicación existente.

## Siguiente paso
[Modela el dominio con data class y enum](02-modela-el-dominio.md).

## Referencias
- https://kotlinlang.org/docs/basic-syntax.html

# Ejercicio final — Mantén y evoluciona FieldFlow

Este ejercicio es la entrega práctica de la lección 17. Trabaja sobre una copia limpia de `learn/es/kotlin/` y conserva evidencia de cada decisión.

## Objetivo

Demostrar que puedes modificar una base Kotlin existente sin una receta paso a paso y mantener sus fronteras JVM/Android.

## Historias obligatorias

1. Añade prioridad `CRITICAL` antes de `HIGH` y protege el orden con pruebas.
2. Convierte un fallo ambiguo de persistencia corrupta en un error explícito y añade una prueba de regresión.
3. Implementa una consulta de órdenes abiertas ordenadas por prioridad y justifica su ubicación.
4. Lleva el cambio a Room sin introducir anotaciones Android en el dominio.
5. Representa guardado/sincronización en progreso sin perder las órdenes ya cargadas en el estado de UI.
6. Modela explícitamente el conflicto offline: una orden completada localmente llega a un servidor donde ya estaba cancelada.

## Restricciones

- No reemplaces el dominio JVM por entidades Room.
- No uses una lista vacía para esconder errores de almacenamiento.
- No resuelvas conflictos con “última escritura gana” sin una justificación explícita.
- No agregues frameworks o capas que no resuelvan una de las historias.
- No persigas 100% de coverage; protege comportamiento relevante y regresiones.

## Evidencia que debes entregar

- diff de código;
- pruebas nuevas/ajustadas para las historias 1–3;
- build/test JVM verde;
- build Android verde cuando el entorno tenga el SDK requerido;
- nota de arquitectura de máximo una página con una decisión tomada y una decisión descartada por sobrearquitectura;
- autoevaluación con la rúbrica de la lección 17.

## Validación mínima

Núcleo JVM, desde `app/`:

```bash
gradle test
gradle run
```

Android, desde `android/`:

```bash
gradle :app:assembleDebug :app:testDebugUnitTest
```

## Antes de ver la solución

Termina primero tu propuesta. Después compara tus decisiones con la [solución de referencia](../solutions/final-fieldflow.md). La solución no es una única respuesta correcta: úsala para comparar responsabilidades, pruebas y trade-offs.

# Evaluación final — Mantén y evoluciona TelemetryTape

Esta evaluación se resuelve sobre la aplicación real del curso. No hay receta paso a paso. Antes de modificar nada, construye y prueba la línea base.

## Escenario

Otro desarrollador necesita extender TelemetryTape sin romper archivos existentes ni convertir la recuperación en una operación destructiva. Tu entrega debe conservar compatibilidad con el formato actual salvo que documentes explícitamente una nueva versión.

## Historias

### 1. Comprende antes de tocar

Explica por escrito, en pocas líneas:

- cómo se representa un registro en disco;
- por qué el formato no depende del padding de una `struct`;
- quién posee cualquier memoria dinámica devuelta por la API;
- cómo se propagan los errores.

### 2. Agrega una capacidad útil

Añade una operación CLI pequeña que reutilice el parser existente. Ejemplos válidos: obtener el último registro de un sensor, contar registros en un intervalo o emitir un resumen de un solo sensor.

Restricciones:

- no cargues todo el archivo en memoria si puedes resolverlo por streaming;
- conserva orden determinista;
- documenta la API pública nueva si la introduces.

### 3. Corrige un bug reproducible

Encuentra o introduce mediante una prueba un caso límite razonable y corrígelo. Debes demostrar primero el fallo y después la regresión verde.

No cuentan cambios cosméticos ni assertions que sólo repitan la implementación.

### 4. Maneja un failure mode

Elige un fallo de entrada o almacenamiento relevante —por ejemplo ruta no escribible, destino de recovery existente/inválido, intervalo fuera de contrato o archivo truncado— y asegúrate de que el comportamiento sea explícito, observable y no destruya el original.

### 5. Agrega una prueba

Añade al menos una prueba automatizada que proteja el comportamiento nuevo o el bugfix. Debe fallar si el defecto reaparece.

### 6. Consulta documentación oficial

Incluye una referencia oficial que hayas usado para una decisión concreta de C, CMake o biblioteca estándar. Resume qué decisión cambió o confirmó esa fuente.

### 7. Diseña el siguiente paso

Propón una mejora que no implementes todavía. Describe:

- beneficio;
- costo/complejidad;
- riesgo de compatibilidad;
- cómo la validarías antes de incorporarla.

## Evidencia requerida

Entrega:

- diff del código;
- prueba(s) nueva(s);
- salida de build y CTest;
- un comando CLI que demuestre la nueva capacidad;
- breve explicación del bug y del failure mode;
- referencia oficial consultada;
- propuesta de mejora futura.

## Rúbrica — 100 puntos

| Área | Puntos | Evidencia esperada |
|---|---:|---|
| Comprensión del formato, ownership y errores | 15 | Explicación correcta y concreta |
| Funcionalidad nueva | 20 | Capacidad útil, integrada y compatible |
| Bugfix | 15 | Reproducción + corrección verificable |
| Manejo de failure mode | 15 | Error explícito y no destructivo |
| Pruebas | 15 | Regresión útil, determinista y verde |
| Documentación oficial | 10 | Fuente oficial conectada con una decisión |
| Diseño y trade-offs | 10 | Mejora futura razonada |

### Interpretación

- **0–59:** aún necesitas práctica guiada antes de mantener este proyecto con autonomía.
- **60–79:** puedes intentar tareas junior acotadas con supervisión y revisión frecuente.
- **80–100:** puedes explicar y defender el proyecto con seguridad razonable y abordar cambios pequeños de forma verificable.

La rúbrica mide este proyecto y estas competencias; no garantiza empleo ni sustituye experiencia profesional.

## Antes de ver la referencia

Haz commit o guarda tu diff. Después compara con [la solución de referencia](../solutions/final-assessment.md), no para copiarla sino para contrastar decisiones.
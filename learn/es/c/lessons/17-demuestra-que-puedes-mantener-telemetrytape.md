# Lección 17 — Demuestra que puedes mantener TelemetryTape

## Qué vas a conseguir

Cerrarás el curso resolviendo una evaluación sin receta paso a paso. El objetivo no es repetir comandos de memoria: es leer una base existente, modificarla con cuidado, proteger el formato y justificar tus decisiones.

## Antes de empezar

Debes haber completado las lecciones 01–16 y los cuatro checkpoints. Ejecuta primero la línea base:

```bash
cd learn/es/c/app
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

No continúes si la línea base ya está roja: primero identifica si el problema es tu entorno o el código.

## La evaluación

Trabaja sobre [Evaluación final — Mantén y evoluciona TelemetryTape](../exercises/final-assessment.md).

No contiene instrucciones de implementación. Sí define comportamiento observable, restricciones y evidencia requerida.

Tendrás que demostrar siete capacidades:

1. leer y explicar el formato `.gtl` y el ownership de la API;
2. agregar una capacidad funcional pequeña sin romper compatibilidad;
3. corregir un bug reproducible;
4. manejar un failure mode explícitamente;
5. agregar al menos una prueba de regresión útil;
6. consultar documentación oficial para fundamentar una decisión;
7. proponer una mejora de diseño con trade-offs claros.

## Cómo trabajar como junior con buenos hábitos

Usa ciclos pequeños:

> reproducir → cambiar una cosa → compilar → probar → observar el diff → continuar

Evita dos atajos peligrosos:

- no cambies el formato binario sin versionarlo y sin pensar en lectores existentes;
- no conviertas recuperación en reparación destructiva del archivo original.

Si introduces memoria dinámica, deja claro quién la libera. Si añades una nueva función pública, documenta parámetros, retorno, ownership y errores.

## Evidencia mínima

Antes de dar por terminada tu solución conserva evidencia de:

```bash
cmake --build build --parallel
ctest --test-dir build --output-on-failure
./build/telemetry_cli diagnose <fixture>
```

Añade el comando específico de tu nueva capacidad y un caso que demuestre el bug corregido.

## Rúbrica

La evaluación usa una rúbrica de 100 puntos incluida en el ejercicio. Una puntuación alta no convierte automáticamente a nadie en desarrollador senior ni garantiza empleo. La meta del curso es que puedas asumir tareas junior razonables con supervisión y explicar cómo llegaste a una solución verificable.

## Cómo hablar de TelemetryTape en una entrevista

Explica primero el problema: registrar y analizar telemetría binaria de forma portable y recuperable. Después resume la arquitectura:

- representación estable en disco separada del layout de memoria;
- parser compartido para validar, consultar y diagnosticar;
- procesamiento streaming para no cargar archivos completos;
- ownership y códigos de error explícitos;
- recuperación hacia una copia, preservando el origen.

Prepárate para responder:

- ¿Por qué no escribiste una `struct` directamente con `fwrite`?
- ¿Dónde vive el ownership de los buffers devueltos por la API?
- ¿Por qué `[start,end)` evita ambigüedad en consultas temporales?
- ¿Cómo distingues archivo inválido, versión futura y truncado?
- ¿Qué trade-off tiene validar todo el stream antes de hacer append?
- ¿Qué harías para soportar archivos mucho más grandes o múltiples versiones?

Una respuesta buena reconoce límites. Por ejemplo: el formato es deliberadamente pequeño, la recuperación conserva sólo el prefijo demostrablemente válido y la matriz actual de CI certifica GCC/Clang sobre Ubuntu, no todas las plataformas C existentes.

## Solución de referencia

Sólo después de intentar la evaluación compara tu enfoque con [la solución de referencia](../solutions/final-assessment.md). No existe una única implementación correcta; la referencia muestra una forma de preservar los contratos del curso.

## Referencias

- [C23 en GCC](https://gcc.gnu.org/projects/c-status.html)
- [Propiedad `C_STANDARD` de CMake](https://cmake.org/cmake/help/latest/prop_tgt/C_STANDARD.html)
- [Biblioteca C en cppreference](https://en.cppreference.com/w/c)

## Siguiente paso

Si puedes resolver la evaluación, explicar tus trade-offs y mantener la suite verde, termina el curso revisando el README y construyendo una copia limpia. Después puedes continuar con el siguiente lenguaje del roadmap o profundizar en debugging, sistemas operativos y arquitectura de computadores.
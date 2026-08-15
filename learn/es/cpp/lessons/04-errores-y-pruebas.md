# Lección 04 — Haz explícitos los errores y prueba comportamiento

## Objetivo

Distinguir errores de entrada, fallos tolerables del filesystem y regresiones de comportamiento mediante pruebas ejecutables.

## Problema

Los árboles reales cambian mientras los recorres y pueden contener rutas sin permisos. Si cualquier incidente produce un resultado silenciosamente incorrecto o un crash, el índice no es confiable.

## Concepto

ThreadSeek distingue dos niveles en este incremento. Una raíz inexistente o que no es directorio invalida toda la operación y lanza `std::invalid_argument`. En cambio, entradas internas inaccesibles se recorren con `skip_permission_denied` y `std::error_code` para continuar sin convertir un subárbol problemático en una excepción global.

Más adelante devolveremos diagnósticos más ricos para que “continuar” no signifique ocultar información relevante.

## Demo

[EJECUTAR]

```bash
./app/build/threadseek /ruta/que/no/existe
printf '%s\n' "$?"
```

La CLI debe terminar con código 1 y explicar que la raíz no existe.

## Código real

`app/tests/indexer_tests.cpp` crea un fixture temporal con tres archivos. Prueba cantidad, bytes totales, orden, búsqueda case-insensitive, consulta vacía y raíz inexistente.

No usamos un framework externo todavía: CTest ejecuta un binario de pruebas pequeño para mantener dependencias mínimas. Si el curso crece hasta necesitar fixtures/diagnósticos más ricos, podremos justificar una librería de testing en vez de agregarla por costumbre.

## Deuda detectada por pruebas

Durante la primera ejecución del curso, el test esperaba 18 bytes aunque los fixtures sumaban 17. El código era correcto; corregimos la expectativa. Este tipo de fallo importa porque TDD no significa obedecer ciegamente un test: significa usar evidencia para decidir cuál contrato es correcto.

## Errores comunes

No atrapes `...` para esconder cualquier problema. No ignores un `error_code` sin decidir si el fallo es fatal o tolerable. No hagas tests dependientes del orden del filesystem: ThreadSeek ordena explícitamente su índice.

## Buenas prácticas

Prueba contratos visibles, no detalles privados. Usa directorios temporales autocontenidos. Limpia fixtures con RAII. Mantén los tests offline y deterministas.

## Ejercicio

Agrega una prueba que pase la ruta de un archivo regular como raíz y compruebe que se rechaza como “no es un directorio”.

## Comprobación

```bash
cmake -S app -B app/build -DCMAKE_BUILD_TYPE=Release
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure
```

## Solución enlazada

Ahora sí estás listo para [Checkpoint 01](../exercises/checkpoint-01.md). La solución está en `../solutions/checkpoint-01.md`.

## Reto

Diseña, sin implementarlo aún, un resultado que pueda devolver archivos indexados y advertencias parciales sin usar un string ambiguo.

## Resumen

El primer vertical slice está completo: compila, indexa, consulta, falla explícitamente y tiene pruebas deterministas.

## Siguiente paso

Completa [Checkpoint 01 — Índice confiable](../exercises/checkpoint-01.md). Después separaremos nuevas fronteras antes de persistir el índice.

## Referencias

Consulta la documentación de `std::filesystem`, `std::error_code` y excepciones de la biblioteca estándar de tu toolchain.

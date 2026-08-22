# Lección 15 — Perfila antes de optimizar

## Objetivo

Comparar secuencial y paralelo con evidencia reproducible sin convertir milisegundos de un runner compartido en requisito funcional.

## Problema

Una versión concurrente puede ser más lenta en árboles pequeños, discos rápidos con cache caliente o máquinas con pocos recursos. Optimizar por intuición favorece complejidad que quizá no compra nada.

## Concepto

`compare_discovery(root, workers)` ejecuta ambas estrategias con `steady_clock` y devuelve dos `DiscoveryReport` más `equivalent`. La equivalencia es gate; la velocidad no. Para profiling real repite sobre datasets representativos, separa cold/warm cache cuando importe y observa CPU, I/O y contención con herramientas de plataforma.

## Aplicación real

Antes de aumentar workers, mide 1, 2, 4 y 8 sobre el mismo árbol. Conserva número de archivos y bytes como control. Si el tiempo deja de mejorar, más concurrencia ya no es la decisión racional.

## Errores comunes

- una sola medición;
- microbenchmarks sobre un árbol diminuto;
- declarar que paralelo siempre gana;
- optimizar una función que no domina el tiempo total;
- usar thresholds absolutos en CI compartido.

## Ejercicio

Construye un pequeño script o tabla manual con tres ejecuciones por configuración. Reporta mediana, no sólo el mejor número, y explica si el costo de coordinación se justifica.

## Comprobación

Los tests sólo exigen `equivalent == true` y tiempos válidos. El análisis de rendimiento se hace con evidencia, no con un assertion frágil.

## Reflexión

¿Qué métrica adicional necesitarías antes de decidir si el cuello de botella está en CPU, syscalls o almacenamiento?

## Siguiente paso

Continúa con [Lección 16 — Endurece portabilidad y entrega](16-endurece-portabilidad-y-entrega.md).

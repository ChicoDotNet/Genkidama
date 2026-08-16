# Lección 09 — Mide la línea base antes de paralelizar

## Objetivo

Medir el descubrimiento secuencial de ThreadSeek sin confundir una observación de rendimiento con una prueba funcional.

## Problema

Decir «multihilo será más rápido» sin medir es una hipótesis. El costo real depende del filesystem, caché, cantidad de archivos, profundidad y hardware.

## Concepto

`measure_discovery` usa `std::chrono::steady_clock` porque necesitamos una duración monotónica. El reporte conserva dos cosas distintas: el resultado funcional y el tiempo observado.

```cpp
const auto report = threadseek::measure_discovery(
    root,
    threadseek::DiscoveryMode::sequential);
```

Una prueba debe verificar que la medición no altera resultados. **No debe exigir** que cierto tiempo sea menor que otro: CI comparte hardware y puede variar.

## Aplicación real

Ejecuta varias veces sobre una carpeta suficientemente grande y registra:

- número de archivos;
- duración;
- tipo de disco o entorno;
- si era la primera ejecución o ya existía caché.

## Errores comunes

- comparar una ejecución fría con una caliente;
- usar `system_clock` para duraciones;
- convertir «paralelo debe ganar» en un assertion;
- optimizar antes de tener resultados correctos.

## Ejercicio

Mide tres veces una misma carpeta y explica por qué los tiempos no son idénticos.

## Comprobación

`cmake --build build --parallel && ctest --test-dir build --output-on-failure`

## Reflexión

¿Qué dato necesitarías antes de afirmar que añadir hilos vale la complejidad?

## Siguiente paso

En la [lección 10](10-particiona-trabajo-con-jthread.md) distribuiremos subdirectorios entre workers acotados.

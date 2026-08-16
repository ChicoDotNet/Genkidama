# Lección 11 — Minimiza estado mutable compartido

## Objetivo

Diseñar concurrencia de forma que la sincronización sea una excepción, no el centro del algoritmo.

## Problema

Si todos los hilos hacen `push_back` sobre el mismo `std::vector`, necesitas coordinación y aumentas el riesgo de carreras y contención.

## Concepto

ThreadSeek da a cada worker su propio `std::vector<FileRecord>`. Ningún worker escribe en el lote de otro. Después de que los `std::jthread` terminan, el hilo coordinador combina los lotes.

```cpp
std::vector<std::vector<FileRecord>> local_batches(actual_workers);
```

La mejor sincronización muchas veces es **no compartir datos mutables durante el trabajo concurrente**.

## Aplicación real

Cada worker recorre una partición de subdirectorios y produce resultados locales. El merge ocurre después del join; por eso no hace falta un mutex alrededor de cada archivo descubierto.

## Errores comunes

- estado global mutable;
- mutex demasiado grande;
- proteger datos que podrían ser locales;
- asumir que ausencia de crash significa ausencia de data race.

## Ejercicio

Describe cómo cambiaría el diseño si cada worker escribiera directamente en `direct_files`. ¿Qué tendrías que proteger y qué costo introducirías?

## Comprobación

Ejecuta las pruebas varias veces. El resultado debe conservar conteo, tamaños y orden.

## Siguiente paso

En la [lección 12](12-compara-sin-perder-determinismo.md) compararemos estrategias sin sacrificar comportamiento observable.

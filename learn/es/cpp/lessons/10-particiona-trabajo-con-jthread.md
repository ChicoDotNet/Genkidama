# Lección 10 — Particiona trabajo con `std::jthread`

## Objetivo

Introducir concurrencia acotada sin abandonar RAII ni crear hilos ilimitados.

## Problema

ThreadSeek puede tener varios subdirectorios independientes. Recorrerlos secuencialmente desaprovecha paralelismo potencial, pero crear un hilo por carpeta no escala.

## Concepto

`discover_files_parallel(root, worker_count)` limita workers y reparte subdirectorios por índice. Cada `std::jthread` se une automáticamente al destruirse.

```cpp
for (std::size_t worker = 0; worker < actual_workers; ++worker) {
    workers.emplace_back([&, worker] {
        for (std::size_t index = worker; index < subdirectories.size(); index += actual_workers) {
            // cada worker procesa su partición
        }
    });
}
```

## Aplicación real

El número solicitado puede ser mayor que las carpetas disponibles. La implementación reduce workers reales a lo útil y rechaza `worker_count == 0` cuando llamas directamente al modo paralelo.

## Errores comunes

- un hilo por archivo;
- `detach()` sin ownership claro;
- olvidar esperar a que terminen los workers;
- asumir que más hilos implica más rendimiento.

## Ejercicio

Prueba 1, 2, 4 y 8 workers sobre el mismo árbol. Registra cuándo deja de mejorar.

## Comprobación

Las pruebas deben seguir demostrando que secuencial y paralelo descubren los mismos archivos.

## Siguiente paso

En la [lección 11](11-minimiza-estado-compartido.md) eliminaremos la necesidad de mutex en el camino principal.

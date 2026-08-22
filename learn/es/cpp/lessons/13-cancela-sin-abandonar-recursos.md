# Lección 13 — Cancela sin abandonar recursos

## Objetivo

Agregar cancelación cooperativa y progreso observable sin matar hilos ni dejar recursos a medias.

## Problema

Un índice grande puede tardar suficiente para que el usuario cambie de opinión. Terminar un proceso o un thread a la fuerza rompe invariantes: archivos temporales, callbacks y workers pueden quedar en estados difíciles de razonar.

## Concepto

C++20+ ofrece `std::stop_source` y `std::stop_token`. El productor solicita detenerse y el trabajo consulta el token en puntos seguros. ThreadSeek introduce `DiscoveryOptions`, `DiscoveryProgress` y `ControlledDiscoveryReport` para que la cancelación sea parte explícita del contrato.

La callback de progreso recibe un snapshot; no recibe referencias a vectores internos ni puede mutar los lotes de los workers. En modo paralelo se serializa la callback, mientras los contadores usan atomics relajados porque sólo necesitamos métricas, no coordinar el algoritmo con ellos.

## Aplicación real

La UI, CLI o servicio que consuma ThreadSeek puede conservar un `std::stop_source`, pasar su token al descubrimiento y solicitar cancelación por Ctrl+C, cierre de ventana o cambio de directorio.

## Errores comunes

- usar `terminate`, señales o excepciones asíncronas como cancelación normal;
- asumir que solicitar stop significa que el trabajo ya terminó;
- compartir el vector de resultados con la callback;
- usar el contador de progreso como sincronización del algoritmo.

## Ejercicio

Solicita stop desde la callback después de observar la primera entrada. Comprueba `cancelled == true` y que al menos un evento de progreso fue publicado.

## Comprobación

```bash
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

## Reflexión

¿Por qué la cancelación cooperativa es más predecible que matar un thread aunque no sea instantánea?

## Siguiente paso

Continúa con [Lección 14 — Tolera un filesystem que cambia](14-tolera-un-filesystem-que-cambia.md).

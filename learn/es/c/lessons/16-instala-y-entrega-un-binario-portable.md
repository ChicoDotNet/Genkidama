# Lección 16 — Instala y entrega un binario portable

## Qué vas a conseguir

Pasarás de "compila en mi carpeta" a una entrega reproducible con CMake.

## El problema

Un ejecutable generado dentro de `build/` no describe qué debe distribuirse. TelemetryTape ahora declara reglas de instalación para la CLI, la biblioteca estática y el header público.

[EJECUTAR]

```bash
cmake -S app -B app/build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure
cmake --install app/build --prefix app/dist
```

El resultado esperado contiene `bin/telemetry_cli`, `lib/` e `include/telemetry.h` según las convenciones de la plataforma.

## Hardening antes de entregar

- warnings como errores en GCC/Clang;
- tests deterministas;
- smoke de CLI;
- formato binario versionado;
- diagnóstico y recuperación explícitos;
- ninguna reparación destructiva automática.

## Tu turno

Resuelve el [Checkpoint 04 — Diagnostica, recupera y entrega](../exercises/checkpoint-04.md).

## Siguiente paso

Continúa con la [Lección 17 — Demuestra que puedes mantener TelemetryTape](17-demuestra-que-puedes-mantener-telemetrytape.md), donde resolverás la evaluación final autónoma del curso.
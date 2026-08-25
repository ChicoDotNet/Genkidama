# Lección 01 — Compila C y ejecuta TelemetryTape

## Qué vas a conseguir

Compilarás una aplicación C real con CMake y crearás tu primer archivo de telemetría.

## El problema

Un archivo fuente `.c` no se ejecuta directamente: el compilador lo transforma en código nativo y el linker reúne las piezas. Necesitamos un proceso repetible, no un comando distinto en cada máquina.

## Concepto

`CMakeLists.txt` declara tres targets: una biblioteca `telemetry`, la CLI `telemetry_cli` y `telemetry_tests`. El estándar se fija explícitamente en C23 y GCC/Clang compilan con warnings como errores.

[DEMO]

```bash
cmake -S app -B app/build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build app/build --parallel
./app/build/telemetry_cli init sample.gtl
```

El comando `init` crea un archivo con un encabezado versionado. Aún no contiene muestras.

## Errores comunes

- Ejecutar desde una ruta distinta y asumir dónde quedó `build/`.
- Ignorar un warning porque “sí compiló”. En C un warning puede señalar conversiones o ownership incorrectos.
- Depender del dialecto predeterminado del compilador. El proyecto pide C23 explícitamente.

## Tu turno

Construye TelemetryTape desde una copia limpia, crea `practice.gtl` y comprueba que el proceso termina con código 0.

## Cómo comprobar tu solución

Ejecuta `ctest --test-dir app/build --output-on-failure`. Aunque todavía no escribiste código, ya tienes una red de seguridad ejecutable.

## Siguiente paso

[Lección 02 — Modela telemetría con tipos de ancho fijo](02-modela-telemetria-con-tipos-fijos.md)

## Referencias

- GCC, C standards support.
- CMake, tutorial y command-line documentation.

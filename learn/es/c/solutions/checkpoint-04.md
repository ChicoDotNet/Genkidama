# Solución — Checkpoint 04

Una secuencia de referencia desde `learn/es/c/`:

```bash
cmake -S app -B app/build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure

./app/build/telemetry_cli init sample.gtl
./app/build/telemetry_cli log sample.gtl 1000 1 100 0
./app/build/telemetry_cli log sample.gtl 2000 2 200 1
./app/build/telemetry_cli log sample.gtl 3000 1 300 2
printf bad >> sample.gtl

./app/build/telemetry_cli diagnose sample.gtl || true
./app/build/telemetry_cli recover sample.gtl recovered.gtl
./app/build/telemetry_cli diagnose recovered.gtl
./app/build/telemetry_cli summary recovered.gtl
./app/build/telemetry_cli diagnose sample.gtl || true

cmake --install app/build --prefix app/dist
```

La parte importante no son los nombres de archivo: es conservar el origen, hacer explícito qué bytes se demostraron válidos y verificar la copia antes de usarla. `recover` no intenta inventar el sufijo perdido.

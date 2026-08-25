# Lección 15 — Recupera sin sobrescribir el original

## Qué vas a conseguir

Recuperarás el prefijo válido de un stream dañado hacia un archivo nuevo.

## Regla de seguridad

`telemetry_recover_valid_prefix` nunca modifica el origen y rechaza usar la misma ruta como destino. Un magic inválido o una versión no soportada abortan antes de crear el destino. Sólo un sufijo truncado o un registro semánticamente inválido se descarta de forma explícita.

[EJECUTAR]

```bash
./app/build/telemetry_cli recover damaged.gtl recovered.gtl
./app/build/telemetry_cli diagnose recovered.gtl
```

La prueba de regresión confirma que el archivo original sigue corrupto y que la copia recuperada vuelve a ser un stream válido.

## Trade-off

Recuperar el prefijo no reconstruye bytes perdidos. Preserva únicamente evidencia demostrablemente válida; cualquier decisión de negocio sobre datos faltantes ocurre fuera de esta función.

## Siguiente paso

En la [lección 16](16-instala-y-entrega-un-binario-portable.md) prepararás una entrega reproducible.

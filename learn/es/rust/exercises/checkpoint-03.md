# Checkpoint 03 — Un snapshot visible debe estar completo

## Escenario
Un compañero propone simplificar `create_snapshot`: crear directamente `snapshots/<nombre>/`, copiar archivos y escribir `manifest.json` al final.

El problema es observable: si la operación falla a mitad, el nombre final ya existe y puede confundirse con una versión válida.

## Tu tarea
Modifica BackupForge para defender estas condiciones sin mirar la solución:

1. un snapshot nuevo se construye fuera del nombre final;
2. antes de publicarse debe tener manifest válido y verificación íntegra;
3. un nombre ya publicado nunca se sobrescribe;
4. un directorio parcial no aparece en `list_snapshots`;
5. agrega al menos una regresión que proteja la frontera.

No implementes retención, locking distribuido ni deduplicación.

## Evidencia esperada
Ejecuta:

```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
```

Explica además por qué “ocultar” carpetas parciales en el listado no sustituye limpiar o reportar una interrupción real.

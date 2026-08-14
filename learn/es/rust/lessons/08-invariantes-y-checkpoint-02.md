# Lección 08 — Invariantes y Checkpoint 02

## Qué vas a conseguir
Cerrarás el segundo bloque defendiendo invariantes que atraviesan incrementalidad, manifests y restore.

## Antes de empezar
Completa la [Lección 07](07-restore-seguro.md).

## El problema
Cuando una aplicación crece, una validación colocada sólo en el parser puede ser omitida por otra ruta pública. Un `Manifest` construido directamente podría incluir una ruta insegura o duplicada y llegar a `verify_backup` o `restore_backup`.

## Concepto
Una frontera pública debe proteger los invariantes que necesita para ser correcta. BackupForge centraliza la validación del manifest y la reutiliza desde carga, verificación y restore.

El segundo principio es **evidencia antes de optimización**: un destino corrupto nunca se marca `reused`; se copia nuevamente desde el origen.

## Demostración
[DEMO] Ejecuta los tests de rutas inseguras y duplicadas. Después abre [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) y resuélvelo sin mirar la solución.

## Código real
Las pruebas integran tipos públicos, filesystem temporal y regresiones de seguridad sin depender de red, reloj ni estado compartido.

## Qué acaba de pasar
El tipo ayuda, pero no sustituye validar datos que pueden venir de disco, JSON o construcción programática.

## Errores comunes
- Validar sólo dentro de `load_manifest`.
- Dar por segura una ruta porque fue deserializada correctamente.
- Contar un archivo corrupto como reutilizado.
- Crear una abstracción genérica cuando una función pequeña expresa mejor la regla.

## Buenas prácticas
Mantén la regla central en un solo lugar, devuelve `Result` con contexto y prueba la API pública que podría saltarse una capa anterior.

## Tu turno — Checkpoint 02
[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md).

## Cómo comprobar
```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo build --release
```

Después ejecuta manualmente:

```bash
cargo run -- create ./origen ./backup
cargo run -- update ./origen ./backup
cargo run -- verify ./backup
cargo run -- restore ./backup ./restaurado
```

## Solución enlazada
Consulta [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md) sólo después de intentar el checkpoint.

## Reto adicional
Explica por qué este backup incremental no es todavía un sistema de snapshots históricos, retención ni deduplicación global.

## Resumen
Ya puedes razonar sobre ownership, errores, checksums, manifests, incrementalidad y restore como un solo contrato verificable.

## Siguiente paso
El siguiente bloque añadirá snapshots/historial sólo si puede preservar estas garantías y producir evidencia útil para diagnóstico.

## Referencias
- https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- https://doc.rust-lang.org/std/collections/struct.HashSet.html
- https://doc.rust-lang.org/std/path/struct.Path.html

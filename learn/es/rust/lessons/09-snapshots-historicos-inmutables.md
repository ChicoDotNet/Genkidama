# Lección 09 — Snapshots históricos inmutables

## Qué vas a conseguir
Convertirás BackupForge de un único backup actual a un repositorio que puede conservar versiones históricas verificables sin sobrescribirlas.

## Antes de empezar
Completa la [Lección 08](08-invariantes-y-checkpoint-02.md) y confirma `cargo test`.

## El problema
Un backup actual sirve para recuperar el último estado, pero no responde “¿cómo estaba este proyecto ayer?”. Sobrescribir una carpeta elimina esa evidencia histórica.

## Concepto
Un **snapshot** es una versión nombrada que, una vez publicada, no se reescribe. BackupForge guarda cada versión en `snapshots/<nombre>/` y mantiene dentro su propio `manifest.json`.

La creación usa dos fases: primero construye y verifica `.<nombre>.partial`; sólo después hace `rename` al nombre final. Así una versión visible representa un backup completo, no una copia a medias.

## Demostración
[DEMO] Crea dos estados distintos del mismo origen:

```bash
cargo run -- snapshot ./origen ./repositorio 2026-08-14-a
# modifica el origen
cargo run -- snapshot ./origen ./repositorio 2026-08-14-b
```

Intenta repetir el primer nombre: BackupForge debe rechazarlo.

## Código real
Revisa `create_snapshot` y `SnapshotSummary` en `app/src/lib.rs`. Observa que la API reutiliza `create_backup` y `verify_backup` en vez de duplicar checksum o manifest.

## Qué acaba de pasar
Introdujiste historial sin romper las garantías existentes: cada snapshot tiene manifest propio, se verifica antes de publicarse y un nombre ya usado no cambia de significado.

## Errores comunes
- Sobrescribir una versión “porque tiene el mismo nombre”.
- Publicar el directorio final antes de terminar la copia.
- Confundir snapshot histórico con deduplicación.
- Usar nombres de snapshot como rutas sin validarlos.

## Buenas prácticas
Haz que los identificadores usados en filesystem sean datos validados. Conserva una ruta de publicación clara: construir → verificar → publicar.

## Tu turno
[PAUSA PARA EJERCICIO] Agrega una prueba que intente usar `../fuera` como nombre y comprueba que no se crea nada fuera de `snapshots/`.

## Cómo comprobar
```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
```

## Solución enlazada
Compara con las pruebas de snapshots sólo después de tu intento.

## Reto adicional
Explica qué cambia si dos procesos intentan crear simultáneamente el mismo nombre. No implementes locking todavía.

## Resumen
Un snapshot es una versión inmutable por identidad; BackupForge sólo la publica después de verificarla.

## Siguiente paso
Continúa con la [Lección 10](10-inspeccion-de-snapshots.md) para inspeccionar historial sin abrir manifests manualmente.

## Referencias
- https://doc.rust-lang.org/std/fs/fn.rename.html
- https://doc.rust-lang.org/std/path/struct.Path.html

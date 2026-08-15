# Lección 01 — Primer backup y ownership visible

## Qué vas a conseguir

Ejecutarás BackupForge y entenderás por qué Rust hace explícito quién posee rutas, buffers y resultados.

## Antes de empezar

Instala Rust estable con `rustup` y confirma `rustc --version` y `cargo --version`.

## El problema

Copiar una carpeta no basta si después no puedes demostrar qué fue copiado.

## Concepto

Rust administra memoria mediante ownership y borrowing. En este vertical las rutas se reciben como `&Path`: la función necesita leerlas, no poseerlas para siempre.

## Demostración

[DEMO] Desde `app/` ejecuta `cargo test` y luego crea dos archivos dentro de una carpeta `origen/`.

## Código real

Ejecuta:

```bash
cargo run -- create ./origen ./backup
```

Inspecciona `src/lib.rs`: `create_backup` recibe referencias a rutas, recorre el árbol de forma determinista y devuelve `Result<Manifest, BackupError>`.

## Qué acaba de pasar

El caller conserva ownership de sus rutas; el core pide prestado lo necesario. Rust impide usar referencias después de que sus valores dejan de existir.

## Errores comunes

- Clonar `PathBuf` por costumbre cuando basta `&Path`.
- Usar `unwrap()` en rutas productivas.
- Mezclar impresión de consola con el algoritmo de backup.

## Buenas prácticas

Mantén ownership simple. Prefiere préstamos cortos y deja CLI/filesystem en fronteras explícitas.

## Tu turno

[PAUSA PARA EJERCICIO] Agrega un subdirectorio y verifica que el backup conserva su ruta relativa.

## Cómo comprobar

```bash
cargo test
cargo run -- create ./origen ./backup
```

## Solución enlazada

La solución del primer checkpoint aparece después de la Lección 04.

## Reto adicional

Explica cuándo una función debería recibir `PathBuf` por valor y cuándo `&Path`.

## Resumen

Ownership no es sintaxis decorativa: hace visible la vida útil de los recursos.

## Siguiente paso

Continúa con [Lección 02 — Result, errores e I/O en fronteras](02-result-errores-y-fronteras.md).

## Referencias

- https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html
- https://doc.rust-lang.org/std/path/struct.Path.html

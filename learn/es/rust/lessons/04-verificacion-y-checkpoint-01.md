# Lección 04 — Verificación y Checkpoint 01

## Qué vas a conseguir
Verificarás un backup y cerrarás el primer checkpoint sin confiar ciegamente en el manifest.

## Antes de empezar
Completa la [Lección 03](03-sha256-y-manifest.md).

## El problema
Una copia sólo es útil si puede demostrarse íntegra.

## Concepto
La verificación recomputa tamaño y SHA-256. El manifest es entrada externa: su versión y sus rutas deben validarse antes de usarse.

## Demostración
[DEMO] Crea un backup, cambia un archivo conservando el mismo tamaño y ejecuta `cargo run -- verify ./backup`.

## Código real
`load_manifest` rechaza versiones desconocidas y rutas con `..`; `verify_backup` devuelve un resultado estructurado.

## Qué acaba de pasar
El core informa diferencias sin conocer stdout ni códigos de proceso.

## Errores comunes
- Unir rutas externas sin validación.
- Confiar sólo en existencia o tamaño.
- Ocultar errores de lectura.

## Buenas prácticas
Valida fronteras y devuelve errores accionables.

## Tu turno
[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md).

## Cómo comprobar
```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
```

## Solución enlazada
Consulta [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md) después del intento.

## Reto adicional
Explica por qué checksum no equivale a autenticidad criptográfica.

## Resumen
Ya puedes crear y verificar una copia local con paths defensivos.

## Siguiente paso
El próximo incremento introducirá incrementalidad sin falsear evidencia.

## Referencias
- https://doc.rust-lang.org/std/path/
- https://doc.rust-lang.org/book/ch12-00-an-io-project.html

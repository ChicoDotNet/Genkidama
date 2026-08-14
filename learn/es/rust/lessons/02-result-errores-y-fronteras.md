# Lección 02 — Result, errores e I/O en fronteras

## Qué vas a conseguir

Propagarás errores recuperables sin ocultarlos ni convertirlos en `panic!`.

## Antes de empezar

Completa la [Lección 01](01-primer-backup-y-ownership.md).

## El problema

Un backup toca filesystem: una ruta puede no existir, un archivo puede no abrirse o un manifest puede estar corrupto.

## Concepto

`Result<T, E>` obliga a modelar éxito y fallo. El operador `?` propaga el error sin perder el tipo original.

## Demostración

[DEMO] Ejecuta `cargo run -- create ./no-existe ./backup` y observa el diagnóstico de I/O.

## Código real

`BackupError` separa I/O, JSON y manifests inválidos. Las conversiones `From` permiten usar `?` sin convertir todo a strings ambiguos.

## Qué acaba de pasar

El core no decide terminar el proceso: devuelve errores. Sólo `main.rs`, la frontera CLI, elige imprimir y devolver código distinto de cero.

## Errores comunes

- `unwrap()` en el camino normal de la aplicación.
- `Box<dyn Error>` dentro de todo el dominio perdiendo semántica.
- Ignorar errores de escritura o lectura.

## Buenas prácticas

Errores accionables, sin fallback silencioso ante pérdida o corrupción de datos.

## Tu turno

[PAUSA PARA EJERCICIO] Provoca una ruta de origen inexistente e identifica qué capa produce y cuál presenta el error.

## Cómo comprobar

```bash
cargo test
cargo run -- create ./no-existe ./backup
```

## Solución enlazada

La solución acumulada se enlaza desde el Checkpoint 01.

## Reto adicional

Diseña una variante de `BackupError` específica para un archivo que desaparece durante el recorrido.

## Resumen

`Result` convierte los fallos previsibles en parte explícita del contrato.

## Siguiente paso

Continúa con [Lección 03 — SHA-256 y manifest determinista](03-sha256-y-manifest.md).

## Referencias

- https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- https://doc.rust-lang.org/std/result/

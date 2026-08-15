# Lección 13 — Un gate profesional y repetible

## Qué vas a conseguir
Vas a convertir los comandos de calidad que ya usamos en un gate local único, pequeño y reproducible.

## Antes de empezar
Completa la [Lección 12](12-fronteras-historial-y-checkpoint-03.md).

## El problema
Cuando cada persona recuerda una combinación distinta de `fmt`, Clippy, tests y build, “en mi máquina funciona” deja de ser evidencia útil.

## Concepto
Un gate profesional no necesita ser pesado. Debe ejecutar, siempre en el mismo orden, las comprobaciones que representan el contrato actual:

```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo build --release
```

`tools/verify.sh` sólo hace explícita esa secuencia. No reemplaza Cargo ni inventa otro sistema de build.

## Demostración
[DEMO] Ejecuta desde la raíz del curso:

```bash
bash tools/verify.sh
```

Provoca después un cambio de formato y observa que el gate falla temprano.

## Código real
El workflow y el desarrollador comparten los mismos comandos. La ventaja no es “tener un script”; es reducir discrepancias entre validación local y CI.

## Qué acaba de pasar
El tooling dejó de depender de memoria humana sin ocultar las herramientas oficiales de Rust.

## Errores comunes
- Silenciar warnings para conseguir verde.
- Ejecutar sólo tests y olvidar formatter/Clippy.
- Crear un wrapper tan complejo que nadie entiende qué valida.
- Usar nightly sólo por una herramienta secundaria.

## Buenas prácticas
Mantén el gate corto, portable y aburrido. Si cambia una comprobación, documenta el motivo y actualiza CI de forma coherente.

## Tu turno
[PAUSA PARA EJERCICIO] Introduce deliberadamente un problema que Clippy detecte, observa el rojo y reviértelo sin desactivar la regla.

## Cómo comprobar
```bash
bash tools/verify.sh
```

## Solución enlazada
La solución es el propio gate del repositorio y la evidencia de CI; no necesitas copiar un segundo script.

## Reto adicional
Explica qué comprobación añadirías antes de publicar un crate público y por qué no es necesaria todavía en BackupForge.

## Resumen
Un gate útil representa contratos reales con herramientas estándar, no una colección ornamental de checks.

## Siguiente paso
Continúa con la [Lección 14 — Debugging basado en evidencia](14-debugging-basado-en-evidencia.md).

## Referencias
- https://doc.rust-lang.org/cargo/commands/cargo-test.html
- https://doc.rust-lang.org/clippy/
- https://github.com/rust-lang/rustfmt

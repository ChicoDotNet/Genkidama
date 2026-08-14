# Lección 12 — Fronteras de historial y Checkpoint 03

## Qué vas a conseguir
Cerrarás el bloque razonando sobre identidad, publicación, inspección e integridad como un solo contrato histórico.

## Antes de empezar
Completa la [Lección 11](11-verificar-y-restaurar-versiones.md).

## El problema
Añadir historial puede convertir rápidamente una herramienta pequeña en un sistema de retención, catálogo, locking y deduplicación. Eso aumenta superficie sin que el problema actual lo exija.

## Concepto
BackupForge mantiene una frontera intencional:

- cada nombre identifica una versión inmutable;
- el snapshot se publica sólo tras verificarse;
- el manifest sigue siendo fuente de verdad;
- el listado es derivado y determinista;
- verificar/restaurar reutiliza las reglas existentes;
- **no** existe todavía borrado automático, política de retención ni deduplicación entre versiones.

Este límite es una decisión de diseño, no una carencia escondida.

## Demostración
[DEMO] Recorre los tests de `app/tests/snapshots.rs` y relaciona cada uno con una garantía observable.

## Código real
La API histórica añade pocas funciones públicas y un solo tipo de resumen. No introduce traits o capas genéricas porque todavía no existe más de un backend ni una necesidad de sustitución.

## Qué acaba de pasar
El curso avanzó de “backup actual” a “historial verificable” sin hacer claims que el código no puede demostrar.

## Errores comunes
- Añadir retención automática sin una política explícita.
- Llamar “deduplicado” a un repositorio que copia bytes por snapshot.
- Ocultar directorios parciales como si nunca pudieran existir tras una interrupción.
- Crear abstracciones anticipadas alrededor de un único filesystem.

## Buenas prácticas
Nombra el alcance con precisión. Agrega abstracciones cuando protejan una frontera real o permitan probar comportamiento que de otro modo quedaría acoplado.

## Tu turno — Checkpoint 03
[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

## Cómo comprobar
```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo build --release
```

## Solución enlazada
Consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md) sólo después de tu intento.

## Reto adicional
Propón una política de retención “conservar últimas 7 versiones” y enumera tres fallas que deberían resolverse antes de implementarla.

## Resumen
BackupForge ya conserva historial verificable con identidad inmutable, publicación defensiva, inspección determinista y restore seguro.

## Siguiente paso
Continúa con la [Lección 13 — Un gate profesional y repetible](13-gate-profesional-y-repetible.md).

## Referencias
- https://doc.rust-lang.org/book/ch12-03-improving-error-handling-and-modularity.html
- https://doc.rust-lang.org/std/fs/

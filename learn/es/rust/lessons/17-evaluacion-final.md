# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir

Demostrarás que puedes leer, modificar, probar y explicar BackupForge sin seguir una receta paso a paso. Esta lección integra ownership, `Result`, filesystem, manifests, checksums, incrementalidad, snapshots, restore, auditoría y tooling profesional de Rust.

## Antes de empezar

Completa la [Lección 16](16-hardening-operativo-y-checkpoint-04.md). Desde la raíz del curso ejecuta:

```bash
bash tools/verify.sh
```

## El problema

Un equipo usa BackupForge para respaldos locales verificables. Necesita una mejora pequeña para recuperar un archivo concreto sin restaurar todo el árbol, pero exige conservar las garantías existentes: rutas seguras, manifest válido, checksums, snapshots inmutables y errores explícitos.

Además, durante una revisión detectan que dos rutas textualmente diferentes podrían normalizar al mismo destino (`docs/a.txt` y `docs/./a.txt`). Eso puede convertir un manifest aparentemente sin duplicados en una restauración ambigua.

No recibirás una lista de archivos, funciones o líneas que debas modificar.

## Concepto

Una evaluación profesional no mide si recuerdas sintaxis. Mide si puedes **leer → formular una hipótesis → escribir una regresión → implementar → diagnosticar → verificar → explicar**.

## Demostración

[DEMO] Antes de cambiar nada, recorre `src/lib.rs`, `src/main.rs`, las pruebas y `tools/verify.sh`. Explica dónde viven hoy las reglas de manifest, dónde ocurre I/O, qué API pública usa la CLI y por qué `verify` y `audit` no significan lo mismo.

## Código real

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md) y resuelve el encargo sobre la misma aplicación canónica. Puedes consultar las lecciones, mensajes del compilador, `cargo doc`, `rustc --explain` y documentación oficial.

No abras la solución antes de completar un intento serio.

## Qué acaba de pasar

Ya no estás siguiendo instrucciones de implementación: estás manteniendo una base Rust existente, descubriendo contratos y decidiendo la frontera correcta para cada cambio.

## Errores comunes

- Restaurar un archivo antes de verificar la evidencia que lo respalda.
- Validar rutas sólo como strings sin pensar en sus componentes.
- Usar `unwrap()` para simplificar errores recuperables.
- Hacer que la biblioteca termine el proceso o escriba en stdout.
- Cambiar silenciosamente la semántica de `verify` o `audit`.
- Corregir un bug sin una prueba que falle antes del arreglo.
- Optimizar hashing o copiar en paralelo sin medir.

## Buenas prácticas

Mantén APIs públicas documentadas con rustdoc, errores recuperables con `Result`, comportamiento determinista, I/O en fronteras y pruebas offline. Conserva `cargo fmt`, Clippy con warnings como errores, tests y release build como un único contrato profesional.

## Tu turno

[PAUSA PARA EJERCICIO] Completa las historias A–F de la evaluación. Después prepara una explicación de cinco minutos sobre arquitectura, integridad, rutas, snapshots, manejo de errores y un tradeoff que hayas aceptado.

## Cómo comprobar

Como mínimo:

```bash
bash tools/verify.sh
```

Además prueba manualmente una restauración selectiva válida, una ruta inexistente o insegura y un backup corrupto. Usa la [`rúbrica final`](../exercises/rubrica-final.md) para autoevaluarte.

## Solución enlazada

Sólo después de tu intento, compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). La referencia describe una dirección válida; no exige código idéntico.

## Reto adicional

Explica qué cambiaría si los snapshots vivieran en almacenamiento remoto y varios procesos pudieran publicar simultáneamente. No implementes un sistema distribuido: identifica identidad, publicación atómica, consistencia, locking/coordinación y autenticidad del manifest.

## Cómo hablar de este proyecto en una entrevista

Cuenta primero el problema: copiar archivos no demuestra que después puedas recuperarlos íntegros. Explica cómo BackupForge genera manifests deterministas, verifica antes de restaurar, valida rutas, reutiliza contenido sólo cuando el destino físico coincide, publica snapshots después de verificarlos y separa `verify` de `audit`.

Después explica tu cambio final: por qué una restauración selectiva sigue necesitando una política explícita de verificación y cómo evitaste rutas semánticamente ambiguas. Reconoce límites: SHA-256 detecta cambios pero no autentica el manifest; los snapshots pueden duplicar bytes y no existe coordinación multi-proceso ni almacenamiento remoto.

Preguntas probables:

- ¿Por qué la biblioteca devuelve `Result` en vez de terminar el proceso?
- ¿Qué diferencia hay entre ownership y borrowing en una función de filesystem?
- ¿Por qué verificas el backup antes de restaurar?
- ¿Cómo impedirías que una ruta del manifest escape del destino?
- ¿Qué diferencia existe entre `verify` y `audit`?
- ¿Por qué un snapshot se construye como parcial antes de `rename`?
- ¿Qué medirías antes de paralelizar hashes o copias?
- ¿Qué cambiarías para un repositorio remoto y varios escritores?

## Resumen

Completar el curso significa poder modificar una aplicación Rust real, demostrar el comportamiento con pruebas y explicar sus decisiones. Es evidencia de preparación inicial; no garantiza contratación.

## Siguiente paso

Repite las áreas débiles de la rúbrica, conserva BackupForge como evidencia y construye una variante propia sin copiar la solución.

## Referencias

- https://doc.rust-lang.org/book/
- https://doc.rust-lang.org/std/result/
- https://doc.rust-lang.org/std/path/enum.Component.html
- https://doc.rust-lang.org/std/fs/
- https://doc.rust-lang.org/cargo/commands/cargo-test.html

# Curso de Rust desde cero — Construye un backup incremental verificable

Rust es un lenguaje de sistemas orientado a seguridad de memoria, rendimiento y control explícito de errores. Este curso parte desde cero y construye **BackupForge**, una herramienta local de backup incremental verificable por SHA-256.

No promete empleo. Rust tiene demanda profesional real, especialmente en sistemas, infraestructura, tooling y componentes de alto rendimiento, pero suele tener menos vacantes junior que lenguajes generalistas.

## Qué vas a construir

BackupForge copia árboles de archivos, genera un `manifest.json` portable, verifica integridad, actualiza sólo contenido que necesita escritura y restaura únicamente después de validar el backup completo. En esta etapa mantiene un único backup actual: no pretende ser todavía un sistema de snapshots históricos, retención o deduplicación global.

## Requisitos

- Rust stable 1.97.1 mediante `rustup`;
- Cargo, rustfmt y Clippy;
- Windows 11 + PowerShell o Linux + bash.

## Instalar / Build / Test / Run

Desde `app/`:

```bash
cargo fmt --check
cargo clippy --all-targets --all-features -- -D warnings
cargo test
cargo build --release
cargo run -- create ./origen ./backup
cargo run -- update ./origen ./backup
cargo run -- verify ./backup
cargo run -- restore ./backup ./restaurado
```

## Lecciones

1. [Primer backup y ownership visible](lessons/01-primer-backup-y-ownership.md)
2. [Result, errores e I/O en fronteras](lessons/02-result-errores-y-fronteras.md)
3. [SHA-256 y manifest determinista](lessons/03-sha256-y-manifest.md)
4. [Verificación y Checkpoint 01](lessons/04-verificacion-y-checkpoint-01.md)
5. [De copia completa a actualización incremental](lessons/05-actualizacion-incremental.md)
6. [Corrupción, borrados y consistencia](lessons/06-corrupcion-borrados-y-consistencia.md)
7. [Restore seguro antes de escribir](lessons/07-restore-seguro.md)
8. [Invariantes y Checkpoint 02](lessons/08-invariantes-y-checkpoint-02.md)

## Checkpoints

- [Checkpoint 01 — rutas seguras](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02 — incrementalidad que no propaga corrupción](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)

## Qué sabrás hacer al terminar

Leer y escribir Rust idiomático sencillo, razonar sobre ownership/borrowing, modelar errores con `Result`, separar core e I/O, probar con Cargo, usar formatter/Clippy, persistir manifests, verificar integridad, hacer actualizaciones incrementales, restaurar con validación previa, modificar una base existente y explicar decisiones de arquitectura.

## FAQ

**¿Necesito saber C/C++?** No. El curso parte de conceptos básicos, aunque experiencia previa ayuda.

**¿Esto sustituye una estrategia real de backup?** No. BackupForge es una aplicación educativa local. Un sistema de producción necesita políticas de retención, cifrado, almacenamiento aislado, restore drills, consistencia ante interrupciones y threat modeling.

**¿Qué significa “incremental” aquí?** Que al actualizar el mismo backup sólo se reescriben archivos nuevos, modificados o corruptos; archivos ya correctos se reutilizan. Aún no significa snapshots históricos ni deduplicación entre múltiples backups.

**¿Por qué SHA-256?** Porque permite detectar cambios de contenido de manera reproducible; no demuestra por sí solo autenticidad ni protege contra un atacante que pueda reemplazar tanto datos como manifest.

## Glosario

- **crate:** unidad de compilación/paquete Rust.
- **ownership:** modelo que determina quién posee un valor y cuándo se libera.
- **borrow:** referencia temporal a un valor sin transferir ownership.
- **Result:** tipo para representar éxito o error recuperable.
- **manifest:** inventario de archivos y metadatos verificables.
- **checksum:** resumen calculado del contenido para detectar cambios.
- **incremental:** actualización que evita reescribir contenido cuya integridad actual ya coincide.
- **restore:** reconstrucción de archivos desde un backup validado.

## Cómo hablar de este proyecto en una entrevista

Explica primero el problema: copiar archivos no basta; necesitas poder demostrar después qué fue respaldado y detectar corrupción. Describe por qué el core calcula hashes y valida manifests mientras filesystem/CLI permanecen en los bordes. Explica que `update_backup` verifica destino antes de reutilizar y que `restore_backup` verifica todo antes de escribir. Reconoce límites: SHA-256 no sustituye cifrado, firma, almacenamiento inmutable ni una política 3-2-1.

## Referencias oficiales

- https://www.rust-lang.org/tools/install
- https://doc.rust-lang.org/book/
- https://doc.rust-lang.org/cargo/
- https://doc.rust-lang.org/stable/clippy/
- https://doc.rust-lang.org/stable/std/

## Siguiente paso

Completa las primeras ocho lecciones y ambos checkpoints. El siguiente bloque añadirá historial/snapshots sólo si puede preservar estas garantías.

# Curso de Rust desde cero — Construye un backup incremental verificable

Rust es un lenguaje de sistemas orientado a seguridad de memoria, rendimiento y control explícito de errores. Este curso parte desde cero y construye **BackupForge**, una herramienta local de backup verificable por SHA-256.

No promete empleo. Rust tiene demanda profesional real, especialmente en sistemas, infraestructura, tooling y componentes de alto rendimiento, pero suele tener menos vacantes junior que lenguajes generalistas.

## Qué vas a construir

BackupForge copia árboles de archivos, genera un `manifest.json` portable y verifica posteriormente que cada archivo siga existiendo con el mismo tamaño y SHA-256. El curso evolucionará ese vertical hacia backups incrementales, restauración segura y diagnóstico.

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
cargo run -- verify ./backup
```

## Lecciones

1. [Primer backup y ownership visible](lessons/01-primer-backup-y-ownership.md)
2. [Result, errores e I/O en fronteras](lessons/02-result-errores-y-fronteras.md)
3. [SHA-256 y manifest determinista](lessons/03-sha256-y-manifest.md)
4. [Verificación y Checkpoint 01](lessons/04-verificacion-y-checkpoint-01.md)

## Qué sabrás hacer al terminar

Leer y escribir Rust idiomático sencillo, razonar sobre ownership/borrowing, modelar errores con `Result`, separar core e I/O, probar con Cargo, usar formatter/Clippy, persistir manifests, verificar integridad, modificar una base existente y explicar decisiones de arquitectura.

## FAQ

**¿Necesito saber C/C++?** No. El curso parte de conceptos básicos, aunque experiencia previa ayuda.

**¿Esto sustituye una estrategia real de backup?** No. BackupForge es una aplicación educativa local. Un sistema de producción necesita políticas de retención, cifrado, almacenamiento aislado, restore drills y threat modeling.

**¿Por qué SHA-256?** Porque permite detectar cambios de contenido de manera reproducible; no demuestra por sí solo autenticidad ni protege contra un atacante que pueda reemplazar tanto datos como manifest.

## Glosario

- **crate:** unidad de compilación/paquete Rust.
- **ownership:** modelo que determina quién posee un valor y cuándo se libera.
- **borrow:** referencia temporal a un valor sin transferir ownership.
- **Result:** tipo para representar éxito o error recuperable.
- **manifest:** inventario de archivos y metadatos verificables.
- **checksum:** resumen calculado del contenido para detectar cambios.

## Cómo hablar de este proyecto en una entrevista

Explica primero el problema: copiar archivos no basta; necesitas poder demostrar después qué fue respaldado y detectar corrupción. Describe por qué el core calcula hashes y valida manifests mientras filesystem/CLI permanecen en los bordes. Reconoce límites: SHA-256 no sustituye cifrado, firma, almacenamiento inmutable ni una política 3-2-1.

## Referencias oficiales

- https://www.rust-lang.org/tools/install
- https://doc.rust-lang.org/book/
- https://doc.rust-lang.org/cargo/
- https://doc.rust-lang.org/stable/clippy/
- https://doc.rust-lang.org/stable/std/

## Siguiente paso

Completa las primeras cuatro lecciones y el Checkpoint 01 antes de ampliar BackupForge.

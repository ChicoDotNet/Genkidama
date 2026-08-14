# Curso de Rust desde cero — Construye un backup incremental verificable

Rust es un lenguaje de sistemas orientado a seguridad de memoria, rendimiento y control explícito de errores. Este curso parte desde cero y construye **BackupForge**, una herramienta local de backup incremental verificable por SHA-256.

No promete empleo. Rust tiene demanda profesional real, especialmente en sistemas, infraestructura, tooling y componentes de alto rendimiento, pero suele tener menos vacantes junior que lenguajes generalistas.

**Estado del curso: completo — 17/17 lecciones.**

## Qué vas a construir

BackupForge copia árboles de archivos, genera un `manifest.json` portable, verifica integridad, actualiza sólo contenido que necesita escritura, restaura únicamente después de validar el backup completo y conserva **snapshots históricos inmutables por nombre**. Cada snapshot tiene su propio manifest y sólo se publica después de verificarse.

Además, `audit` compara lo declarado con lo realmente presente para detectar entradas inesperadas sin cambiar el contrato histórico de `verify`.

La herramienta sigue siendo deliberadamente local: no implementa retención automática, locking multi-proceso, almacenamiento remoto ni deduplicación global entre snapshots.

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
cargo run -- audit ./backup
cargo run -- restore ./backup ./restaurado
cargo run -- snapshot ./origen ./repositorio 2026-08-14-a
cargo run -- snapshots ./repositorio
cargo run -- verify-snapshot ./repositorio 2026-08-14-a
cargo run -- restore-snapshot ./repositorio 2026-08-14-a ./restaurado
```

Desde la raíz del curso también puedes ejecutar el gate completo:

```bash
bash tools/verify.sh
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
9. [Snapshots históricos inmutables](lessons/09-snapshots-historicos-inmutables.md)
10. [Inspección determinista de snapshots](lessons/10-inspeccion-de-snapshots.md)
11. [Verificar y restaurar una versión](lessons/11-verificar-y-restaurar-versiones.md)
12. [Fronteras de historial y Checkpoint 03](lessons/12-fronteras-historial-y-checkpoint-03.md)
13. [Un gate profesional y repetible](lessons/13-gate-profesional-y-repetible.md)
14. [Debugging basado en evidencia](lessons/14-debugging-basado-en-evidencia.md)
15. [Medir antes de optimizar](lessons/15-medir-antes-de-optimizar.md)
16. [Hardening operativo y Checkpoint 04](lessons/16-hardening-operativo-y-checkpoint-04.md)
17. [Evaluación final sin receta](lessons/17-evaluacion-final.md)

## Checkpoints

- [Checkpoint 01 — rutas seguras](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02 — incrementalidad que no propaga corrupción](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)
- [Checkpoint 03 — un snapshot visible debe estar completo](exercises/checkpoint-03.md) · [solución](solutions/checkpoint-03.md)
- [Checkpoint 04 — el backup contiene algo no declarado](exercises/checkpoint-04.md) · [solución](solutions/checkpoint-04.md)

## Evaluación final

- [Encargo autónomo — BackupForge](exercises/evaluacion-final.md)
- [Rúbrica de 100 puntos](exercises/rubrica-final.md)
- [Solución de referencia](solutions/evaluacion-final.md) — consúltala sólo después de tu intento.

La evaluación pide extender la misma base existente, corregir un bug de integridad de rutas, conservar errores idiomáticos, añadir regresiones, consultar documentación oficial y defender el diseño. La solución de referencia es una dirección posible, no una receta obligatoria.

## Qué sabrás hacer al terminar

Leer y escribir Rust idiomático sencillo, razonar sobre ownership/borrowing, modelar errores con `Result`, separar core e I/O, probar con Cargo, usar formatter/Clippy, persistir manifests, verificar integridad, hacer actualizaciones incrementales, restaurar con validación previa, conservar versiones históricas verificables, diagnosticar diferencias entre contenido declarado/observado, medir antes de optimizar, modificar una base existente y explicar decisiones de arquitectura.

## FAQ

**¿Necesito saber C/C++?** No. El curso parte de conceptos básicos, aunque experiencia previa ayuda.

**¿Esto sustituye una estrategia real de backup?** No. BackupForge es una aplicación educativa local. Un sistema de producción necesita políticas de retención, cifrado, almacenamiento aislado, restore drills, coordinación entre procesos y threat modeling.

**¿Qué significa “incremental” aquí?** Que al actualizar el mismo backup sólo se reescriben archivos nuevos, modificados o corruptos; archivos ya correctos se reutilizan.

**¿Qué significa “snapshot” aquí?** Una versión histórica nombrada que no se sobrescribe. Actualmente cada snapshot conserva sus propios bytes; no implica deduplicación global ni una política automática de retención.

**¿Cuál es la diferencia entre `verify` y `audit`?** `verify` comprueba tamaño y SHA-256 de lo declarado en el manifest. `audit` añade una comprobación del conjunto observado para reportar entradas no declaradas. Ninguno sustituye firma, antivirus o controles del host.

**¿Por qué SHA-256?** Porque permite detectar cambios de contenido de manera reproducible; no demuestra por sí solo autenticidad ni protege contra un atacante que pueda reemplazar tanto datos como manifest.

## Glosario

- **crate:** unidad de compilación/paquete Rust.
- **ownership:** modelo que determina quién posee un valor y cuándo se libera.
- **borrow:** referencia temporal a un valor sin transferir ownership.
- **Result:** tipo para representar éxito o error recuperable.
- **manifest:** inventario de archivos y metadatos verificables.
- **checksum:** resumen calculado del contenido para detectar cambios.
- **incremental:** actualización que evita reescribir contenido cuya integridad actual ya coincide.
- **snapshot:** versión histórica identificada por un nombre inmutable.
- **audit:** comparación entre contenido declarado y contenido observado.
- **restore:** reconstrucción de archivos desde un backup validado.

## Cómo hablar de este proyecto en una entrevista

Explica primero el problema: copiar archivos no basta; necesitas poder demostrar después qué fue respaldado, detectar corrupción y recuperar una versión concreta. Describe por qué el core calcula hashes y valida manifests mientras filesystem/CLI permanecen en los bordes. Explica que `update_backup` verifica destino antes de reutilizar, que `restore_backup` verifica todo antes de escribir y que un snapshot se construye como `.partial`, se verifica y sólo entonces se publica mediante `rename`.

Añade la distinción operacional: `verify` protege entradas declaradas mientras `audit` reporta también contenido inesperado. Explica por qué no cambiaste silenciosamente el contrato existente y por qué medir rendimiento no justifica debilitar checksums.

Reconoce límites reales: SHA-256 no sustituye cifrado o firma; los snapshots actuales pueden duplicar bytes y no existe retención automática, locking multi-proceso ni almacenamiento remoto.

Preguntas probables:

- ¿Por qué `Result` es mejor que terminar el proceso dentro de la biblioteca?
- ¿Por qué verificas el archivo físico antes de marcarlo como reutilizado?
- ¿Qué evita que un nombre de snapshot escape del repositorio?
- ¿Por qué el listado se deriva de manifests en vez de mantener otro índice?
- ¿Qué diferencia hay entre `verify` y `audit`?
- ¿Qué medirías antes de paralelizar el hashing?
- ¿Qué garantías da y cuáles no da `fs::rename` en este diseño?
- ¿Cómo tratarías rutas equivalentes o no canónicas en un manifest?
- ¿Qué cambiarías para almacenamiento remoto o varios procesos concurrentes?

## Referencias oficiales

- https://www.rust-lang.org/tools/install
- https://doc.rust-lang.org/book/
- https://doc.rust-lang.org/cargo/
- https://doc.rust-lang.org/stable/clippy/
- https://doc.rust-lang.org/stable/std/

## Siguiente paso

Usa la [evaluación final](exercises/evaluacion-final.md) y su [rúbrica](exercises/rubrica-final.md) para identificar áreas débiles. Después construye una variante propia de BackupForge o un proyecto Rust pequeño donde debas decidir las fronteras sin copiar esta solución.

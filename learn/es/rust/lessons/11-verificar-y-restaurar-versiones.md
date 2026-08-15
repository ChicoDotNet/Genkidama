# Lección 11 — Verificar y restaurar una versión

## Qué vas a conseguir
Verificarás una versión histórica por nombre y la restaurarás sin relajar la regla “verificar todo antes de escribir”.

## Antes de empezar
Completa la [Lección 10](10-inspeccion-de-snapshots.md).

## El problema
El historial sólo es valioso si puedes demostrar que una versión sigue íntegra antes de recuperarla. Elegir una carpeta antigua no debe saltarse las validaciones construidas en las primeras ocho lecciones.

## Concepto
`verify_snapshot` y `restore_snapshot` son adaptadores pequeños: resuelven un nombre seguro a una carpeta y reutilizan `load_manifest`, `verify_backup` y `restore_backup`.

La composición es deliberada. Una nueva capacidad no necesita una nueva definición de integridad.

## Demostración
```bash
cargo run -- verify-snapshot ./repositorio 2026-08-14-a
cargo run -- restore-snapshot ./repositorio 2026-08-14-a ./restaurado
```

Después altera manualmente un archivo dentro del snapshot y repite ambos comandos.

## Código real
La prueba `corrupt_snapshot_is_reported_and_not_restored` modifica bytes conservando la ruta y comprueba dos efectos: la verificación detecta el archivo y restore no escribe salida.

## Qué acaba de pasar
El historial heredó el contrato de seguridad del backup actual en vez de crear un camino alternativo más débil.

## Errores comunes
- Restaurar primero y verificar después.
- Confiar en el nombre del snapshot como prueba de integridad.
- Capturar un error y continuar con una restauración parcial.
- Duplicar lógica de checksum para la ruta histórica.

## Buenas prácticas
Prefiere composición de contratos ya probados. Si dos caminos prometen la misma garantía, ambos deben terminar en la misma regla central.

## Tu turno
Corrompe un snapshot de prueba y demuestra que el directorio de restore queda sin el archivo esperado.

## Cómo comprobar
```bash
cargo test corrupt_snapshot_is_reported_and_not_restored
```

## Solución enlazada
Consulta `app/tests/snapshots.rs` sólo después de escribir tu regresión.

## Reto adicional
Diseña cómo reportarías todos los snapshots corruptos sin detenerte en el primero. No implementes aún un scanner global.

## Resumen
Versionar no cambia la definición de integridad: manifest + tamaño + SHA-256 siguen siendo la evidencia antes de restore.

## Siguiente paso
Cierra el bloque con la [Lección 12](12-fronteras-historial-y-checkpoint-03.md).

## Referencias
- https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html
- https://doc.rust-lang.org/std/result/enum.Result.html

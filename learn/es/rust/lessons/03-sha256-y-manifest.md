# Lección 03 — SHA-256 y manifest determinista

## Qué vas a conseguir

Calcularás checksums por streaming y construirás un manifest reproducible.

## Antes de empezar

Completa la [Lección 02](02-result-errores-y-fronteras.md).

## El problema

El nombre y tamaño de un archivo no prueban que su contenido siga intacto.

## Concepto

SHA-256 resume contenido arbitrario en un digest de 256 bits. BackupForge lee por bloques para no cargar archivos completos en memoria. El recorrido se ordena por nombre para producir manifests deterministas.

## Demostración

[DEMO] Revisa la prueba del vector conocido `abc` y compárala con el digest publicado por la implementación.

## Código real

`sha256_file` actualiza un `Sha256` con bloques de 64 KiB. `create_backup` genera `ManifestEntry { path, bytes, sha256 }` y escribe `manifest.json` sólo después de copiar los archivos.

## Qué acaba de pasar

El backup tiene ahora evidencia verificable de qué bytes pretendía conservar.

## Errores comunes

- Usar timestamp como prueba de integridad.
- Confiar sólo en tamaño.
- Generar orden no determinista desde `read_dir`.
- Confundir checksum con firma/autenticidad.

## Buenas prácticas

Ordena datos serializados cuando el orden no es semántico y prueba con vectores conocidos.

## Tu turno

[PAUSA PARA EJERCICIO] Cambia cuatro bytes por otros cuatro bytes y razona por qué el tamaño no detecta el cambio pero SHA-256 sí.

## Cómo comprobar

```bash
cargo test sha256_matches_known_vector
```

## Solución enlazada

La solución acumulada se enlaza desde el Checkpoint 01.

## Reto adicional

Explica por qué almacenar el manifest junto al backup permite detectar corrupción accidental pero no necesariamente un atacante con escritura total.

## Resumen

Checksums + manifest convierten una copia opaca en una copia verificable.

## Siguiente paso

Continúa con [Lección 04 — Verificación y Checkpoint 01](04-verificacion-y-checkpoint-01.md).

## Referencias

- https://docs.rs/sha2/
- https://doc.rust-lang.org/std/io/trait.Read.html

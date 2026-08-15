# Lección 10 — Inspección determinista de snapshots

## Qué vas a conseguir
Listarás versiones históricas con evidencia útil: nombre, cantidad de archivos y bytes, en orden determinista.

## Antes de empezar
Completa la [Lección 09](09-snapshots-historicos-inmutables.md).

## El problema
Tener carpetas históricas no basta. Un operador necesita saber qué versiones existen y su tamaño aproximado sin leer JSON manualmente ni depender del orden del filesystem.

## Concepto
`list_snapshots` deriva un `SnapshotSummary` desde el manifest de cada versión. No crea una segunda base de datos: nombre, archivos y bytes son información reconstruible.

La salida se ordena por nombre. El orden determinista hace que pruebas, scripts y diagnósticos sean reproducibles.

## Demostración
[EJECUTAR]

```bash
cargo run -- snapshots ./repositorio
```

La salida usa columnas simples:

```text
2026-08-14-a    12    48320
2026-08-14-b    13    50102
```

## Código real
Revisa `list_snapshots` y `manifest_summary`. Los directorios parciales que comienzan con `.` no aparecen como versiones válidas.

## Qué acaba de pasar
Añadiste observabilidad sin introducir estado derivado persistido. La fuente de verdad continúa siendo cada manifest.

## Errores comunes
- Confiar en el orden de `read_dir`.
- Contar un directorio `.partial` como snapshot terminado.
- Guardar un índice duplicado sin necesidad.
- Presentar bytes totales como “espacio único usado” cuando aún no existe deduplicación.

## Buenas prácticas
Deriva vistas cuando el costo es pequeño y la fuente de verdad ya contiene la información. Ordena explícitamente cuando la salida forma parte del contrato.

## Tu turno
Agrega dos snapshots en orden inverso y prueba que `list_snapshots` devuelve nombres ordenados.

## Cómo comprobar
```bash
cargo test lists_snapshots_in_deterministic_name_order
```

## Solución enlazada
Consulta las pruebas de `app/tests/snapshots.rs` después de intentar el ejercicio.

## Reto adicional
¿Qué métrica adicional podrías derivar del manifest sin leer el contenido de los archivos? Explica su utilidad antes de implementarla.

## Resumen
La inspección útil puede ser una vista derivada, determinista y barata sobre manifests ya existentes.

## Siguiente paso
Continúa con la [Lección 11](11-verificar-y-restaurar-versiones.md).

## Referencias
- https://doc.rust-lang.org/std/fs/fn.read_dir.html
- https://doc.rust-lang.org/std/vec/struct.Vec.html

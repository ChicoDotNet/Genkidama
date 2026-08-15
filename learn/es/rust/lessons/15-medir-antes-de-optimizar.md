# Lección 15 — Medir antes de optimizar

## Qué vas a conseguir
Vas a razonar sobre costo de I/O y hashing sin introducir optimizaciones que debiliten integridad.

## Antes de empezar
Completa la [Lección 14](14-debugging-basado-en-evidencia.md).

## El problema
SHA-256 obliga a leer bytes. Los snapshots además copian bytes. Es tentador “optimizar” confiando sólo en tamaño o timestamp, pero ya demostramos que dos archivos del mismo tamaño pueden contener datos distintos.

## Concepto
Antes de optimizar responde:

- ¿cuántos archivos procesamos?
- ¿cuántos bytes representa el manifest?
- ¿qué proporción fue reutilizada/copiada en `update`?
- ¿el cuello está en hashing, lectura, escritura o latencia del almacenamiento?
- ¿la optimización conserva las garantías de integridad?

BackupForge ya expone conteos y bytes suficientes para formular una hipótesis inicial. Una medición temporal local sirve para comparar escenarios, no para prometer rendimiento universal.

## Demostración
[DEMO] Genera un fixture local representativo y compara:

```bash
time cargo run --release -- create ./fixture ./backup
time cargo run --release -- update ./fixture ./backup
```

Repite varias veces antes de interpretar diferencias. En PowerShell puedes usar `Measure-Command`.

## Código real
`sha256_file` lee por streaming con un buffer acotado. `update_backup` sólo reutiliza cuando origen, manifest previo y copia física coinciden. No eliminamos esa segunda verificación para ganar velocidad sin evidencia.

## Qué acaba de pasar
Separamos una medición útil de una micro-optimización especulativa.

## Errores comunes
- Medir una sola ejecución y tratarla como benchmark.
- Comparar debug contra release.
- Optimizar timestamps sacrificando checksums.
- Añadir paralelismo sin medir presión sobre disco.
- Convertir “más rápido en mi SSD” en promesa de producto.

## Buenas prácticas
Optimiza sólo una hipótesis respaldada por datos y conserva una regresión que proteja la semántica previa.

## Tu turno
[PAUSA PARA EJERCICIO] Crea dos fixtures —muchos archivos pequeños y pocos archivos grandes— y documenta qué diferencia observas sin afirmar una causa que no mediste.

## Cómo comprobar
El gate funcional sigue siendo:

```bash
bash tools/verify.sh
```

La medición es evidencia complementaria, no un test determinista de CI.

## Solución enlazada
No hay un número “correcto”: conserva tus comandos, tamaños y observaciones para poder repetir el experimento.

## Reto adicional
Propón cómo experimentarías con hashing paralelo sin saturar el almacenamiento y qué invariantes deben seguir verdes.

## Resumen
Rendimiento profesional significa medir una carga representativa y preservar corrección mientras se optimiza.

## Siguiente paso
Continúa con la [Lección 16 — Hardening operativo y Checkpoint 04](16-hardening-operativo-y-checkpoint-04.md).

## Referencias
- https://doc.rust-lang.org/std/time/
- https://doc.rust-lang.org/cargo/commands/cargo-run.html
- https://doc.rust-lang.org/std/io/trait.Read.html

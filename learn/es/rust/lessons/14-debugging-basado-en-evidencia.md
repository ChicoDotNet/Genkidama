# Lección 14 — Debugging basado en evidencia

## Qué vas a conseguir
Vas a diagnosticar corrupción e inconsistencias sin convertir cada síntoma en una reescritura del sistema.

## Antes de empezar
Completa la [Lección 13](13-gate-profesional-y-repetible.md).

## El problema
Un usuario dice: “el backup está raro”. Eso puede significar contenido alterado, archivo faltante, manifest inválido o incluso archivos adicionales que el manifest nunca declaró.

## Concepto
Reduce el problema con evidencia:

1. carga y valida el manifest;
2. ejecuta `verify` para comprobar archivos declarados;
3. inspecciona el filesystem real;
4. distingue *mismatch* de *contenido inesperado*;
5. corrige la causa mínima.

La nueva operación `audit` cubre los pasos 2–4 para un backup actual.

## Demostración
[DEMO]

```bash
cargo run -- audit ./backup
```

Agrega manualmente `backup/injected.txt` sin tocar el manifest y vuelve a ejecutar. `verify` por sí solo protege lo declarado; `audit` además reporta la entrada extra.

## Código real
El audit vive en la frontera CLI. El core de manifests y checksums permanece estable porque el nuevo requisito es operativo: comparar el conjunto esperado con el conjunto observado.

## Qué acaba de pasar
No confundimos “los archivos del manifest coinciden” con “el directorio contiene exactamente lo que espero”.

## Errores comunes
- Borrar archivos sospechosos antes de obtener evidencia.
- Tratar cualquier error como corrupción.
- Imprimir contenido de archivos en diagnósticos.
- Cambiar el algoritmo de backup cuando el problema está en una frontera operativa.

## Buenas prácticas
Reporta rutas y categorías, no contenido sensible. Mantén orden determinista para que dos ejecuciones sean comparables.

## Tu turno
[PAUSA PARA EJERCICIO] Añade un archivo extra anidado y extiende la regresión para exigir su ruta relativa exacta.

## Cómo comprobar
```bash
cargo test
cargo run -- audit <backup>
```

## Solución enlazada
Compara tu prueba con las regresiones privadas del binario sólo después de intentarlo.

## Reto adicional
Diseña cómo auditarías snapshots sin permitir que un nombre recibido del usuario escape de `repository/snapshots/`.

## Resumen
Debugging profesional comienza separando clases de falla y reuniendo evidencia suficiente antes de modificar código.

## Siguiente paso
Continúa con la [Lección 15 — Medir antes de optimizar](15-medir-antes-de-optimizar.md).

## Referencias
- https://doc.rust-lang.org/std/fs/
- https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html

# Lección 16 — Hardening operativo y Checkpoint 04

## Qué vas a conseguir
Vas a cerrar el bloque profesional distinguiendo integridad criptográfica, contenido inesperado y límites operativos de BackupForge.

## Antes de empezar
Completa la [Lección 15](15-medir-antes-de-optimizar.md).

## El problema
Un manifest correcto no autoriza automáticamente cualquier objeto presente en el directorio. Un archivo agregado fuera del flujo puede ser accidente, residuo o señal de manipulación.

## Concepto
El hardening de esta etapa mantiene dos contratos separados:

- `verify`: comprueba tamaño y checksum de cada entrada declarada;
- `audit`: exige además que no existan entradas no declaradas (excepto `manifest.json`).

No seguimos symlinks durante el inventario del audit; cualquier entrada no regular termina como contenido inesperado en vez de convertirse en una ruta implícitamente confiable.

## Demostración
[DEMO] Crea un backup, ejecútalo limpio, agrega un archivo extra y compara:

```bash
cargo run -- verify ./backup
cargo run -- audit ./backup
```

La diferencia es intencional y debe poder explicarse.

## Código real
La regresión crea un backup válido, inyecta un archivo que no está en el manifest y exige que `audit` lo reporte sin clasificarlo como checksum mismatch.

## Qué acaba de pasar
El sistema ahora puede expresar dos niveles de evidencia sin cambiar silenciosamente el significado histórico de `verify`.

## Errores comunes
- Hacer que `verify` cambie de semántica sin avisar.
- Seguir symlinks durante una auditoría de seguridad.
- Borrar extras automáticamente.
- Mostrar contenido sensible para “ayudar” al diagnóstico.
- Presentar audit como defensa completa contra malware o adversarios con control del host.

## Buenas prácticas
Haz fallar de forma explícita, reporta rutas relativas y deja la remediación como decisión consciente del operador.

## Tu turno — Checkpoint 04
[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución.

## Cómo comprobar
```bash
bash tools/verify.sh
cargo run -- audit <backup>
```

## Solución enlazada
Consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md) sólo después de completar tu intento.

## Reto adicional
Explica qué controles necesitarías antes de afirmar que un repositorio remoto es resistente a un atacante con capacidad de modificar manifest y archivos.

## Resumen
BackupForge distingue ahora verificación de contenido declarado, auditoría estricta del directorio y seguridad de host, que sigue fuera de alcance.

## Siguiente paso
Continúa con la [Lección 17 — Evaluación final sin receta](17-evaluacion-final.md).

## Referencias
- https://doc.rust-lang.org/std/fs/struct.DirEntry.html
- https://doc.rust-lang.org/std/fs/struct.FileType.html
- https://doc.rust-lang.org/std/path/

# Lección 15 — Respalda antes de recuperar

## Qué vas a conseguir

Vas a copiar los documentos originales a otra carpeta antes de intentar cualquier recuperación manual.

## El problema

Una herramienta que “repara” sobre el único original puede convertir un incidente recuperable en pérdida de datos. Incluso un archivo corrupto es evidencia útil.

## Concepto

`QuoteBackupService.CreateBackup` copia todos los `.quote.json`, incluidos los corruptos, en orden determinista y rechaza usar el mismo directorio como origen y destino. No interpreta ni normaliza los archivos durante el respaldo.

## Código real

Ver [QuoteBackupService.vb](../app/QuoteDesk.Core/QuoteBackupService.vb).

## Tu turno

Crea dos archivos, ejecuta el backup y demuestra byte por byte que el origen quedó intacto.

## Siguiente paso

Continúa con [Lección 16 — Publica un artefacto WinForms reproducible](16-publica-un-artefacto-winforms-reproducible.md).

## Referencias
- [File.Copy](https://learn.microsoft.com/dotnet/api/system.io.file.copy)

# Lección 09 — Lista y busca cotizaciones guardadas

## Qué vas a conseguir

Vas a convertir una carpeta de archivos `.quote.json` en un catálogo consultable sin cargar datos inválidos como si fueran correctos.

## El problema

Guardar una cotización sirve poco si luego necesitas recordar el nombre exacto del archivo. QuoteDesk necesita descubrir documentos y buscar por cliente, pero un archivo corrupto no debe derribar todo el listado.

## Concepto

`QuoteCatalog` enumera archivos en orden determinista, delega la reconstrucción a `QuoteFileStore` y devuelve dos colecciones: entradas válidas e incidencias observables.

## Código real

Ver implementación: [QuoteCatalog.vb](../app/QuoteDesk.Core/QuoteCatalog.vb)

## Tu turno

Guarda dos cotizaciones y comprueba que buscar una parte del nombre del cliente devuelve sólo la esperada.

## Cómo comprobar tu solución

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

## Resumen

Buscar documentos es una operación de dominio/aplicación; no necesita depender de controles WinForms.

## Siguiente paso

Continúa con [Lección 10 — Haz visibles los errores de persistencia](10-haz-visibles-los-errores-de-persistencia.md).

## Referencias
- [Directory.GetFiles](https://learn.microsoft.com/dotnet/api/system.io.directory.getfiles)

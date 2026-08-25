# Solución de referencia — Checkpoint 04

Una solución razonable combina `QuoteDiagnostics`, `QuoteDiagnosticsFormatter` y `QuoteBackupService` sin introducir datos del cliente en la salida operativa ni modificar archivos durante la observación.

Valida el núcleo con:

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

Y el artefacto Windows con:

```powershell
dotnet publish .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release -r win-x64 --self-contained false -o .\publish\quotedesk
```

La solución correcta puede usar nombres distintos, pero debe mantener estas propiedades: diagnóstico read-only, ausencia de PII innecesaria, backup hacia otra ruta y una prueba de regresión para el defecto investigado.

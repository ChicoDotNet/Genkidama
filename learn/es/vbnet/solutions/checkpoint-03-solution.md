# Solución de referencia — Checkpoint 03

Una solución razonable usa `QuoteFileStore` para preparar los archivos, `QuoteCatalog.Search` para descubrir entradas e incidencias y `QuoteInvoiceService` + `InvoiceFileStore` para crear el snapshot durable.

La idea importante no es un nombre de clase concreto: **un archivo corrupto sigue observable, una cotización válida puede continuar su flujo y la factura resultante no depende de volver a mutar el draft original**.

Ejecuta:

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

Compara tu diseño con `QuoteCatalogTests.vb` e `InvoiceFileStoreTests.vb`, pero conserva tu propia explicación de trade-offs.

# Solución de referencia — Evaluación final QuoteDesk

No existe una única implementación correcta. Una dirección razonable es mantener la vigencia dentro de `QuoteDraft`, representarla con un tipo de fecha explícito y comprobarla dentro de `Approve()`. La persistencia puede evolucionar de manera compatible: un campo ausente conserva el significado anterior, mientras una fecha presente se valida al reconstruir el dominio.

La UI sólo captura/presenta el valor; no decide si una cotización vencida puede aprobarse. Esa regla pertenece al núcleo portable y debe tener pruebas que no necesiten WinForms.

Para integridad, conserva dos capas: el objeto `InvoiceDocument` rechaza estados incoherentes creados por callers y `InvoiceFileStore` transforma defectos del JSON externo en una frontera de `InvalidDataException`. El backup debe prevalidar colisiones antes de copiar para no dejar un conjunto parcial y nunca debe modificar el origen.

Una evidencia mínima razonable incluye:

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
dotnet build .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release
dotnet publish .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release -r win-x64 --self-contained false -o .\publish\quotedesk
```

Para almacenamiento compartido, sustituir primero la frontera de persistencia/catálogo es preferible a reescribir el dominio. Habría que añadir identidad estable, control de concurrencia, migraciones, backup, observabilidad y una política explícita para no registrar datos de clientes.

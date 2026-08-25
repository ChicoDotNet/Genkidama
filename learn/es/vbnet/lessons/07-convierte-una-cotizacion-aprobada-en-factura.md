# Lección 07 — Convierte una cotización aprobada en factura

## Qué vas a conseguir

Vas a crear una frontera explícita entre cotización y factura sin hacer que la interfaz gráfica decida reglas de negocio.

## El problema

Una factura no debe aparecer desde cualquier borrador. Necesitamos demostrar que la cotización está aprobada y copiar un snapshot estable de cliente, partidas e importes.

## Concepto

`QuoteInvoiceService.CreateInvoice` acepta sólo `QuoteStatus.Approved`. El resultado, `InvoiceDocument`, conserva una copia inmutable de las partidas y no modifica la cotización origen.

## Código real

Ver implementación: [InvoiceDocument.vb](../app/QuoteDesk.Core/InvoiceDocument.vb)

## Errores comunes

- Permitir facturar un borrador.
- Reutilizar la lista mutable interna de la cotización.
- Hacer que un botón WinForms contenga la regla de aprobación.

## Tu turno

Intenta facturar primero un borrador y después la misma cotización ya aprobada. Comprueba el folio y el total resultante.

## Cómo comprobar tu solución

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

## Resumen

La transición cotización → factura es una regla del dominio y produce un documento independiente.

## Siguiente paso

Continúa con [Lección 08 — Guarda y recupera cotizaciones con JSON](08-guarda-y-recupera-cotizaciones-con-json.md).

## Referencias

- [Classes and objects in Visual Basic](https://learn.microsoft.com/dotnet/visual-basic/programming-guide/language-features/objects-and-classes/)

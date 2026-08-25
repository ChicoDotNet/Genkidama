# Solución — Checkpoint 02

Una solución válida mantiene el flujo dentro de las fronteras existentes en vez de mover reglas a WinForms.

```vbnet
Dim quote = New QuoteDraft With {
    .CustomerName = "Cliente demo",
    .TaxRate = 0.16D
}

quote.AddLine(New QuoteLine("Análisis", 1, 100D))
quote.AddLine(New QuoteLine("Implementación", 1, 300D))
quote.ReplaceLine(0, New QuoteLine("Descubrimiento", 2, 75D))
quote.RemoveLine(1)
quote.Approve()

Dim store = New QuoteFileStore()
store.Save("quote.json", quote)
Dim restored = store.Load("quote.json")
Dim invoice = QuoteInvoiceService.CreateInvoice(restored, "F-CHK-02")
```

## Qué deberías poder explicar

- `QuoteDraft` posee y valida la mutación.
- `Approve()` congela el borrador.
- `QuoteInvoiceService` exige aprobación y no modifica la cotización.
- `QuoteFileStore` es I/O: serializa DTOs versionados y reconstruye el dominio.
- La prueba debe observar comportamiento, no campos privados.

Tu implementación puede organizar el código de otra manera si conserva estos contratos y pasa las pruebas.

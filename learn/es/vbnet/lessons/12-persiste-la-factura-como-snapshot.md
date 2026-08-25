# Lección 12 — Persiste la factura como snapshot

## Qué vas a conseguir

Vas a guardar una factura independiente de la cotización que la originó.

## El problema

Una factura no debe ser una vista mutable de la cotización. Tras aprobar y convertir, el documento necesita conservar su propio folio, partidas y totales.

## Concepto

`InvoiceDocument` ya es inmutable. `InvoiceFileStore` persiste ese snapshot con schema versionado y escritura temporal antes de reemplazo. Cargar vuelve a validar el contrato externo.

## Código real

Ver implementación: [InvoiceFileStore.vb](../app/QuoteDesk.Core/InvoiceFileStore.vb)

## Tu turno

Aprueba una cotización, crea `F-001`, guarda la factura, cárgala y compara folio, total y partidas.

## Resumen

La frontera cotización→factura ya no termina en memoria: produce un documento durable y explícitamente versionado.

## Siguiente paso

Completa el [Checkpoint 03](../exercises/checkpoint-03.md). Después continúa con [Lección 13 — Diagnostica sin exponer datos del cliente](13-diagnostica-sin-exponer-datos-del-cliente.md).

## Referencias
- [JsonSerializer](https://learn.microsoft.com/dotnet/api/system.text.json.jsonserializer)

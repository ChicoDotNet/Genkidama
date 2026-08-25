# Lección 11 — Trata JSON como datos externos no confiables

## Qué vas a conseguir

Vas a validar archivos antes de convertirlos en objetos de negocio utilizables.

## El problema

Un archivo local también puede estar truncado, editado a mano o provenir de una versión futura. Deserializar no equivale a validar.

## Concepto

Los stores comprueban versión, colecciones obligatorias y contratos del dominio. `JsonException` y datos inválidos terminan como `InvalidDataException`: el caller recibe una frontera clara de datos externos defectuosos.

## Código real

Revisa [QuoteFileStore.vb](../app/QuoteDesk.Core/QuoteFileStore.vb) e [InvoiceFileStore.vb](../app/QuoteDesk.Core/InvoiceFileStore.vb).

## Tu turno

Crea un JSON roto y verifica que la aplicación no construye silenciosamente una cotización o factura vacía.

## Siguiente paso

Continúa con [Lección 12 — Persiste la factura como snapshot](12-persiste-la-factura-como-snapshot.md).

## Referencias
- [System.Text.Json](https://learn.microsoft.com/dotnet/standard/serialization/system-text-json/overview)

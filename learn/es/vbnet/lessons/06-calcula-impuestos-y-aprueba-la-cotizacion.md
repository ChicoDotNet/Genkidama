# Lección 06 — Calcula impuestos y aprueba la cotización

## Qué vas a conseguir

Vas a modelar la tasa, el impuesto, el total y el estado `Draft → Approved` con reglas explícitas.

## El problema

Una cotización necesita cliente, partidas y una tasa válida antes de quedar aprobada. Después de aprobarla, modificarla silenciosamente dañaría el contrato del documento.

## Concepto

`QuoteStatus` hace visible el estado. La tasa se expresa como fracción y se valida entre 0 y 1. El impuesto se redondea a dos decimales de forma determinista.

`Approve()` falla si falta cliente o no hay partidas. Después, `EnsureEditable()` protege las mutaciones.

## Código real

Ver implementación: [QuoteDraft.vb](../app/QuoteDesk.Core/QuoteDraft.vb)

## Errores comunes

- Guardar el total como estado mutable en lugar de derivarlo.
- Aprobar una cotización vacía.
- Editar un documento aprobado sin transición explícita.

## Tu turno

Crea una cotización con subtotal `100.03`, tasa `0.16` y comprueba impuesto y total. Después apruébala e intenta agregar una partida.

## Cómo comprobar tu solución

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

## Resumen

El estado y las reglas viven en el dominio; la UI observa el resultado en vez de duplicar la lógica.

## Siguiente paso

Continúa con [Lección 07 — Convierte una cotización aprobada en factura](07-convierte-una-cotizacion-aprobada-en-factura.md).

## Referencias

- [Decimal data type](https://learn.microsoft.com/dotnet/visual-basic/language-reference/data-types/decimal-data-type)
- [Math.Round](https://learn.microsoft.com/dotnet/api/system.math.round)

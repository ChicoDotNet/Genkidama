# Lección 05 — Edita una cotización sin romper sus reglas

## Qué vas a conseguir

Vas a modificar y eliminar partidas existentes manteniendo las reglas dentro de `QuoteDraft`, no en controles WinForms.

## Antes de empezar

Completa el Checkpoint 01. La cotización ya puede agregar partidas y calcular subtotal.

## El problema

Una cotización real cambia: el cliente ajusta cantidades, alcance o precios. Si la UI modifica listas internas directamente, las reglas quedan repartidas y son difíciles de probar.

## Concepto

`QuoteDraft` conserva el ownership de su colección. `ReplaceLine` y `RemoveLine` reciben un índice cero-basado, validan límites y sólo operan mientras el documento siga editable.

[DEMO] Abre `app/QuoteDesk.Core/QuoteDraft.vb` y localiza `ReplaceLine`, `RemoveLine` y `EnsureEditable`.

## Código real

Ver implementación: [QuoteDraft.vb](../app/QuoteDesk.Core/QuoteDraft.vb)

La colección expuesta por `Lines` sigue siendo de sólo lectura. El llamador pide una operación; no recibe permiso para mutar `_lines`.

## Errores comunes

- Exponer `List(Of QuoteLine)` públicamente.
- Ignorar índices fuera de rango y esconder un bug.
- Permitir cambios después de aprobar la cotización.

## Tu turno

Agrega una partida, reemplázala y después elimínala. Comprueba subtotal y cantidad de partidas en una prueba.

## Cómo comprobar tu solución

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

## Solución

Compara después de intentarlo con las pruebas de `QuoteWorkflowTests.vb`.

## Resumen

La mutación pertenece al objeto que posee el estado. Eso mantiene reglas, errores y pruebas en una sola frontera.

## Siguiente paso

Continúa con [Lección 06 — Calcula impuestos y aprueba la cotización](06-calcula-impuestos-y-aprueba-la-cotizacion.md).

## Referencias

- [Properties in Visual Basic](https://learn.microsoft.com/dotnet/visual-basic/programming-guide/language-features/procedures/property-procedures)
- [Collections](https://learn.microsoft.com/dotnet/standard/collections/)

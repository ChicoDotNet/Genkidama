# Lección 03 — Calcula una cotización con objetos

## Qué vas a conseguir

Combinarás varios objetos en un `QuoteDraft` y calcularás un subtotal sin duplicar estado.

## El problema

Una cotización contiene muchas partidas. Necesitamos una colección interna modificable, pero no queremos que cualquier consumidor pueda borrar o insertar elementos saltándose las reglas del objeto.

`QuoteDraft` conserva una `List(Of QuoteLine)` privada y expone una vista `IReadOnlyList(Of QuoteLine)`. El subtotal se calcula con LINQ sobre las partidas actuales.

## Código real

Ver implementación: [`../app/QuoteDesk.Core/QuoteDraft.vb`](../app/QuoteDesk.Core/QuoteDraft.vb).

## Tu turno

Crea un test con tres partidas y comprueba el subtotal. Después intenta explicar por qué una propiedad pública `List(Of QuoteLine)` sería una frontera más débil.

## Siguiente paso

[Lección 04 — Separa la vista con un presenter](04-separa-la-vista-con-un-presenter.md).
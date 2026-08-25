# Lección 04 — Separa la vista con un presenter

## Qué vas a conseguir

Harás que el comportamiento principal pueda probarse sin abrir una ventana.

## El problema

Si toda la validación vive dentro de eventos `Button.Click`, probar reglas obliga a levantar WinForms. QuoteDesk usa una vista pasiva: `MainForm` expone entradas y eventos; `QuotePresenter` interpreta la intención, valida y actualiza el borrador.

Esto responde a una necesidad concreta de testabilidad de UI. No necesitas memorizar el nombre de un patrón para usar bien la separación.

## Código real

- [`IQuoteView.vb`](../app/QuoteDesk.Core/IQuoteView.vb)
- [`QuotePresenter.vb`](../app/QuoteDesk.Core/QuotePresenter.vb)
- [`MainForm.vb`](../app/QuoteDesk.WinForms/MainForm.vb)

El presenter no referencia `TextBox`, `Button` ni `Form`. Las pruebas usan una vista falsa y ejecutan el mismo comportamiento que dispara la UI.

## Tu turno

Provoca una cantidad inválida y comprueba que el presenter muestra un error sin modificar el borrador. Después resuelve el [Checkpoint 01](../exercises/checkpoint-01.md).

## Siguiente paso

Continúa con [Lección 05 — Edita una cotización sin romper sus reglas](05-edita-una-cotizacion-sin-romper-sus-reglas.md).

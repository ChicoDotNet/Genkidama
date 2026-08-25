# Lección 14 — Depura con evidencia reproducible

## Qué vas a conseguir

Vas a transformar una sospecha en evidencia pequeña, determinista y repetible.

## El problema

“En mi máquina funciona” no explica por qué un catálogo tiene tres documentos válidos y otro sólo dos. Necesitas separar observación, hipótesis y corrección.

## Concepto

Usa el ciclo **reproducir → observar → reducir a una prueba → reparar → revalidar**. `QuoteDiagnosticsFormatter` produce las mismas claves en el mismo orden, de modo que una captura de soporte se puede comparar sin depender de controles WinForms ni de textos de clientes.

## Código real

Revisa `QuoteOperationsTests.Diagnostics_AggregatesCountsWithoutExposingCustomerNames`.

## Buenas prácticas

- Conserva la entrada que reproduce el fallo.
- No arregles datos antes de entender el defecto.
- Añade una prueba de regresión cerca del comportamiento.
- Reejecuta tanto el núcleo portable como el build WinForms.

## Siguiente paso

Continúa con [Lección 15 — Respalda antes de recuperar](15-respalda-antes-de-recuperar.md).

## Referencias
- [Visual Studio debugger documentation](https://learn.microsoft.com/visualstudio/debugger/)

# Lección 02 — Modela una partida con tipos explícitos

## Qué vas a conseguir

Entenderás `String`, `Integer`, `Decimal`, propiedades de sólo lectura y validación de constructor.

## El problema

Una cotización no puede aceptar cantidad cero, precio negativo o descripción vacía y esperar que la interfaz lo arregle después.

`QuoteLine` protege esas reglas en el objeto que representa la partida. `Decimal` se usa para importes porque evita muchos problemas de representación binaria de `Double` en dinero.

## Código real

Ver implementación: [`../app/QuoteDesk.Core/QuoteLine.vb`](../app/QuoteDesk.Core/QuoteLine.vb).

Observa que `LineTotal` se deriva de cantidad × precio unitario: no hay un segundo valor mutable que pueda quedar inconsistente.

## Tu turno

Agrega una prueba que demuestre que un precio negativo es rechazado. No cambies producción hasta haber observado qué contrato quieres proteger.

## Siguiente paso

[Lección 03 — Calcula una cotización con objetos](03-calcula-una-cotizacion-con-objetos.md).
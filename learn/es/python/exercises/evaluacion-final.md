# Evaluación final — Evoluciona LedgerMatch sin receta

## Historia A — Tolerancia configurable

Agrega `--tolerance IMPORTE`:

- default `0` conserva exactitud actual;
- usa `Decimal`, nunca `float`;
- `matched` cuando `abs(payment_total - invoice_total) <= tolerance`;
- conserva la diferencia real;
- negativo/no decimal → código 2 y mensaje útil;
- regla independiente del CLI;
- tests de límite, dentro, fuera e inválido.

## Historia B — Bug de duplicados

El parser puede aceptar `F-100` y `f-100` como distintos. Para este encargo el ID será case-insensitive después de `strip()`. Corrige el bug preservando la primera grafía y agrega regresión.

## Historia C — Conserva contratos

Demuestra idempotencia, JSON/CSV, hardening, wheel y compatibilidad de CLI sin `--tolerance`.

## Historia D — Consulta documentación

Incluye una nota breve con al menos una fuente oficial y qué decisión verificaste.

## Historia E — Diseño

Escribe 150–300 palabras: dónde pertenece tolerancia, si debe persistirse, qué migración implicaría y si la harías ahora.

## Entrega

Código, pruebas, comandos ejecutados, explicación de arquitectura, nota de documentación y reflexión.

## Comprobación

```bash
python -m pytest
python -m pip wheel --no-deps . -w dist
ledgermatch examples/invoices.csv --db final.db --tolerance 0.50
```

No abras [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md) antes de completar tu intento.

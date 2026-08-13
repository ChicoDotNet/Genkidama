# Solución de referencia — Evaluación final

Una dirección posible para tolerancia:

```python
def reconcile(parsed, *, tolerance=Decimal("0")):
    if not tolerance.is_finite() or tolerance < 0:
        raise ValueError("La tolerancia debe ser finita y no negativa.")
    ...
    status = MatchStatus.MATCHED if abs(difference) <= tolerance else MatchStatus.DIFFERENCE
```

La CLI convierte `--tolerance` a `Decimal` y transforma formato/rango inválido en código 2.

Para duplicados conserva display y normaliza sólo la llave:

```python
invoice_id = (row.get("invoice_id") or "").strip()
invoice_key = invoice_id.casefold()
...
seen_invoice_ids.add(invoice_key)
```

Prueba tolerancia 0, límite exacto, dentro/fuera y el duplicado case-insensitive.

Persistir tolerancia sería útil para auditoría, pero también cambia la semántica de idempotencia: mismo archivo con políticas distintas puede representar corridas distintas. Documentaría esa migración de esquema/clave antes de introducirla silenciosamente.

Valida con:

```bash
python -m pytest
python -m pip wheel --no-deps . -w dist
```

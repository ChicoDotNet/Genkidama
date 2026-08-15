# Solución de referencia — Checkpoint 01

Una solución pequeña mantiene un conjunto de identificadores aceptados dentro de `read_invoices`:

```python
seen_invoice_ids: set[str] = set()
```

Después de normalizar `invoice_id`, pero antes de construir `InvoiceRecord`, comprueba si ya existe:

```python
if invoice_id in seen_invoice_ids:
    row_issues.append(
        ValidationIssue(row_number, "invoice_id", "El identificador está duplicado.")
    )
```

Sólo cuando la fila completa es válida agrega el identificador al conjunto:

```python
if row_issues:
    issues.extend(row_issues)
    continue

seen_invoice_ids.add(invoice_id)
records.append(...)
```

Esto evita registrar como “ocupado” un identificador cuya primera aparición estaba inválida por otro motivo.

Una prueba útil crea dos filas válidas con el mismo ID y comprueba:

- un solo `record`;
- un `ValidationIssue` para `invoice_id`;
- la fila duplicada no altera los totales conciliados.

No es necesario introducir una clase repositorio ni modificar `reconcile`: el defecto pertenece a la importación del archivo actual.

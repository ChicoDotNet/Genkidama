# Solución de referencia — Checkpoint 02

No existe una única estructura correcta. La referencia de LedgerMatch separa tres decisiones.

## 1. Agrupar es análisis

`analytics.py` usa un `dict` cuya clave es `customer.casefold()`. Conserva aparte la primera escritura del nombre y produce `CustomerSummary` con `Decimal`.

La función recibe un `ReconciliationSummary`; no vuelve a abrir el CSV.

## 2. Formatear es presentación

`reporting.py` recibe el resumen y, cuando `include_customers=True`, agrega una sección `Por cliente:`. Devuelve un `str`, así que puede probarse sin `print`.

## 3. La CLI sólo conecta la opción

`__main__.py` declara:

```python
parser.add_argument("--by-customer", action="store_true")
```

Y pasa ese booleano a `format_report`.

## Prueba útil

`test_report_can_show_selected_detail_and_customer_totals` demuestra que dos facturas del mismo cliente se suman con precisión y que el reporte conserva el filtro de detalle por separado.

## Qué comparar con tu solución

- ¿la agrupación puede probarse sin terminal ni archivos?;
- ¿los importes siguen siendo `Decimal`?;
- ¿la opción es realmente opcional?;
- ¿puedes explicar por qué cada módulo cambió?;
- ¿evitaste crear abstracciones que no resuelven ningún problema actual?

Si tu implementación responde bien esas preguntas, no necesita parecerse línea por línea a esta referencia.

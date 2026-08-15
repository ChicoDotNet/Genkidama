# Checkpoint 02 — Resume la conciliación por cliente

LedgerMatch ya filtra el detalle. Ahora una persona responsable de cobranza necesita ver, opcionalmente, un resumen agregado por cliente.

## Trabajo

Agrega una opción `--by-customer` que, sin cambiar el CSV, muestre para cada cliente aceptado:

- número de facturas;
- total facturado;
- total pagado;
- diferencia `pagado - facturado`.

La agrupación debe tratar diferencias de mayúsculas/minúsculas como el mismo cliente y conservar un nombre legible para mostrar.

## Restricciones

- No leas nuevamente el CSV para construir el reporte.
- No uses `float` para los totales.
- No pongas la agrupación dentro del parser.
- Mantén el comportamiento existente cuando `--by-customer` no se especifica.
- Agrega al menos una prueba automatizada del cálculo o del reporte.

## Cómo comprobar

```bash
python -m pytest
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --by-customer
```

Debes poder explicar qué responsabilidad tiene el módulo donde colocaste la agrupación y por qué.

Sólo después de un intento completo abre [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

# Checkpoint 04 — Protege entradas y destinos de reporte

Una opción `--force` permite reemplazar reportes existentes, pero no debe volver legítimo destruir el archivo fuente ni hacer que JSON y CSV compitan por el mismo destino.

## Trabajo

Implementa validación previa a persistencia/exportación:

- ningún reporte puede resolver a la misma ruta que el CSV de entrada;
- JSON y CSV no pueden resolver a la misma ruta entre sí;
- aplica incluso con `--force`;
- rechazo antes de modificar SQLite/reportes;
- código de salida 2 desde CLI;
- pruebas que demuestren preservación de la fuente.

No compares sólo strings crudos: `./datos.csv` y `datos.csv` pueden representar el mismo archivo.

## Cómo comprobar

```bash
python -m pytest
ledgermatch examples/invoices.csv --json examples/invoices.csv --force
```

La entrada debe permanecer intacta. Después compara con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).

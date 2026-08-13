# Lección 11 — Reportes CSV y JSON

## Qué vas a conseguir

LedgerMatch podrá producir archivos para personas y para otras herramientas: texto para terminal, JSON estructurado y CSV tabular.

## El problema

Un `print()` legible es útil para una persona. No siempre es el mejor contrato para:

- cargar resultados en otra aplicación;
- abrir detalle en una hoja de cálculo;
- conservar evidencia;
- automatizar un flujo posterior.

Copiar la conciliación tres veces sería peor. Necesitamos varias representaciones del mismo resultado.

## Concepto: datos primero, representación después

El dominio ya produce `ReconciliationSummary` y `ReconciliationLine`. `reporting.py` convierte esos objetos en formatos de salida.

La regla sigue siendo:

```text
parser → reconciler → analytics → reporting
```

JSON y CSV no vuelven a decidir si una factura coincide.

## Demostración

[EJECUTAR]

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv \
  --db demo.db \
  --only-differences \
  --json differences.json \
  --csv differences.csv
```

Abre ambos archivos.

El JSON conserva un bloque `summary` y una lista `lines`. El CSV contiene una fila por línea seleccionada.

## Código real: JSON

Los importes se serializan como strings:

```python
"invoice_total": str(line.record.invoice_total)
```

Esto mantiene explícita la representación decimal. Un consumidor puede decidir después cómo modelarla.

`ensure_ascii=False` conserva texto legible como nombres con acentos.

## Código real: CSV

`csv.writer` controla delimitadores, comillas y saltos:

```python
with Path(path).open("w", encoding="utf-8", newline="") as stream:
    writer = csv.writer(stream)
```

No construimos una fila con `",".join(...)`: un nombre de cliente puede contener comas y debe escapar correctamente.

## Qué acaba de pasar

Una sola conciliación puede tener múltiples vistas. Esa separación hace que agregar JSON no requiera tocar `parser.py`, y agregar CSV no requiera reescribir `reconcile()`.

## Errores comunes

### Serializar `Decimal` como `float` por comodidad

Puede cambiar la representación que elegiste deliberadamente para dinero.

### Construir CSV a mano

Comas, comillas y saltos de línea convierten rápido un `join` en un parser defectuoso.

### Hacer que cada exportador vuelva a filtrar

La selección ya pertenece a `analytics.select_lines()`. Reporting recibe lo que debe representar.

### Mezclar `print` dentro de funciones de transformación

Dificulta probar el contenido sin capturar consola.

## Buenas prácticas

- conserva una fuente de verdad para reglas;
- usa módulos estándar de serialización;
- define encoding;
- prueba leyendo otra vez el archivo producido;
- distingue resumen global de detalle filtrado.

## Tu turno

Crea un JSON sólo con diferencias de `Cliente Uno` y verifica que:

1. el resumen siga describiendo toda la corrida;
2. `lines` contenga únicamente el detalle seleccionado;
3. los importes sigan siendo strings decimales.

## Cómo comprobar

```bash
python -m pytest tests/test_reporting_exports.py -v
```

## Solución

`test_json_report_preserves_decimal_text` y `test_csv_report_writes_selected_lines` muestran cómo validar el archivo producido desde el punto de vista de un consumidor.

## Reto adicional

¿Cuándo preferirías JSON sobre CSV y cuándo al revés? Responde pensando en estructura, interoperabilidad y quién será el consumidor.

## Resumen

- un mismo resultado puede tener varias representaciones;
- JSON conserva estructura;
- CSV favorece interoperabilidad tabular;
- `Decimal` se exporta deliberadamente como texto;
- los módulos estándar evitan reinventar serialización.

## Siguiente paso

En la [Lección 12](12-configuracion-logging-y-checkpoint.md) sacaremos configuración fuera del código, añadiremos diagnóstico con `logging` y cerrarás el tercer bloque con un checkpoint autónomo.

## Referencias

- [`json`](https://docs.python.org/3.14/library/json.html)
- [`csv`](https://docs.python.org/3.14/library/csv.html)

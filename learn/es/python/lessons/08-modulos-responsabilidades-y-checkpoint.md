# Lección 8 — Módulos, responsabilidades y segundo checkpoint

## Qué vas a conseguir

Vas a leer LedgerMatch como un paquete de responsabilidades pequeñas y cerrarás el bloque agregando una vista por cliente sin convertir `__main__.py` en una función gigante.

## El problema

Si parsing, conciliación, filtros, formato y `print` viven juntos, cada nueva salida exige tocar el mismo bloque. Funciona al principio y después cuesta probar cada decisión por separado.

## Concepto

Un módulo Python es simplemente un archivo importable. La división actual responde a razones concretas:

- `models.py`: datos compartidos;
- `parser.py`: CSV y validación de entrada;
- `reconciler.py`: regla de conciliación;
- `analytics.py`: selección y agrupación;
- `reporting.py`: texto determinista;
- `__main__.py`: argumentos, I/O y código de salida.

No buscamos un archivo por clase. Buscamos fronteras que podamos explicar.

## Demostración

`format_report` devuelve `str` en vez de imprimir. Por eso una prueba puede verificar el reporte sin interceptar consola.

`__main__.py` conserva el efecto:

```python
print(format_report(summary, selected_lines=selected, include_customers=args.by_customer))
```

## Código real

Ejecuta:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --only-differences --by-customer
```

Verás el resumen general, el detalle filtrado y una sección agregada por cliente.

## Qué acaba de pasar

La salida ganó una capacidad sin enseñar a `parser.py` a imprimir ni a `reporting.py` a leer archivos. Cada módulo tiene una razón dominante de cambio.

## Errores comunes

- crear decenas de módulos de tres líneas sin una frontera real;
- usar `__init__.py` como depósito de lógica;
- hacer que `reporting.py` vuelva a calcular reglas que ya existen;
- ocultar dependencias mediante imports globales confusos.

## Buenas prácticas

Mantén imports explícitos, docstrings en superficies públicas, funciones puras cuando el problema lo permite y efectos en bordes visibles.

## Tu turno — Checkpoint 02

Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución. La historia pide un resumen opcional por cliente y exige una prueba.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

```bash
python -m pytest
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --by-customer
```

## Solución

Cuando termines tu intento, compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md). No necesitas copiar su estructura si la tuya mantiene responsabilidades claras y pasa los criterios.

## Reto adicional

Explica qué módulo debería cambiar si mañana el reporte se escribiera además en JSON y cuáles deberían permanecer intactos.

## Resumen

- módulos útiles separan razones de cambio;
- formato y salida no son lo mismo;
- una función pura de reporting puede probarse sin consola;
- el checkpoint exige extender la aplicación sin receta línea por línea.

## Siguiente paso

En la [Lección 9](09-persistencia-sqlite.md) introduciremos persistencia con `sqlite3`. Ahora sí existe información derivada que vale la pena conservar entre ejecuciones.

## Referencias

- [Módulos de Python](https://docs.python.org/3.14/tutorial/modules.html)

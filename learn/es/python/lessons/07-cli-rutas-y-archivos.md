# Lección 7 — Argumentos de CLI, rutas y archivos

## Qué vas a conseguir

Vas a usar LedgerMatch como una herramienta, no como un script que exige editar código: elegirás archivo, cliente y si quieres ver sólo diferencias desde la línea de comandos.

## Antes de empezar

Desde `app/`, prueba primero:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --only-differences
```

En PowerShell usa `$env:PYTHONPATH = 'src'` antes del mismo comando.

## El problema

Hardcodear `examples/invoices.csv` o una variable `customer = "Acme"` obliga a modificar el programa para cada ejecución. Una herramienta útil separa código de parámetros de uso.

## Concepto

`argparse` pertenece a la biblioteca estándar y convierte argumentos en datos tipados. `pathlib.Path` representa rutas sin concatenar separadores manualmente.

## Demostración

En `__main__.py`:

```python
parser.add_argument("csv_file", type=Path)
parser.add_argument("--customer")
parser.add_argument("--only-differences", action="store_true")
```

Después la frontera CLI llama a la lógica que ya existe:

```python
selected = select_lines(
    summary,
    customer=args.customer,
    only_differences=args.only_differences,
)
```

## Código real

Prueba:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --customer "Cliente Uno"
```

Y combina criterios:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --customer "Cliente Uno" --only-differences
```

## Qué acaba de pasar

La terminal decide *qué* quiere consultar; `select_lines` decide *cómo* filtrar. `argparse` no se filtra hacia las reglas de negocio.

## Errores comunes

- leer `sys.argv` manualmente en varias funciones;
- concatenar rutas como strings con `/` o `\\`;
- devolver código 0 cuando la entrada es estructuralmente inválida;
- capturar cualquier `Exception` y convertirlo en “archivo no encontrado”.

## Buenas prácticas

Valida lo que corresponde a cada frontera: `argparse` forma de invocación, parser estructura del CSV y reglas puras significado del dato.

## Tu turno

Ejecuta `python -m ledgermatch --help`. Luego prueba un archivo inexistente y observa el código de salida. Finalmente usa ambos filtros y comprueba que el detalle contiene sólo lo pedido.

## Cómo comprobar

```bash
python -m pytest app/tests/test_cli.py -v
```

La prueba usa `monkeypatch` para controlar argumentos y `capsys` para observar salida sin lanzar un proceso externo.

## Solución

La CLI del repositorio conserva `main() -> int`; eso permite probar el código de salida además del texto.

## Reto adicional

¿Por qué `Path` mejora portabilidad aunque no elimine todos los errores posibles del sistema de archivos?

## Resumen

- una CLI separa configuración de código;
- `argparse` documenta y valida invocación;
- `Path` representa rutas de forma portable;
- los códigos de salida son parte del contrato de una herramienta.

## Siguiente paso

En la [Lección 8](08-modulos-responsabilidades-y-checkpoint.md) separaremos presentación de cálculo y cerrarás el bloque con una mejora de reporte sin receta.

## Referencias

- [`argparse`](https://docs.python.org/3.14/library/argparse.html)
- [`pathlib`](https://docs.python.org/3.14/library/pathlib.html)

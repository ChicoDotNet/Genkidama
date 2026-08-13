# Lección 1 — Tu primera conciliación en ejecución

## Qué vas a conseguir

Antes de estudiar sintaxis vas a ejecutar **LedgerMatch** y convertir un CSV pequeño en un resultado útil: cuántas facturas coinciden con su pago y cuáles requieren revisión.

## Antes de empezar

Necesitas Python 3.14. Comprueba:

```bash
python --version
```

Trabaja desde `learn/es/python/app`.

## El problema

Una empresa recibe una exportación con cinco datos por fila: identificador de factura, cliente, fecha, total facturado y total pagado. Abrir el archivo y revisar importe por importe funciona con cuatro registros; con miles deja de ser un proceso confiable.

La primera meta no es “aprender variables”. Es lograr que una computadora repita una comparación sin cansarse.

## Concepto

Python ejecuta módulos. `python -m ledgermatch` busca el paquete `ledgermatch` y ejecuta su archivo `__main__.py`.

Como todavía no hemos empaquetado la aplicación, indicamos que el código fuente vive en `src` mediante `PYTHONPATH`.

## Demostración

Linux/macOS:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv
```

PowerShell:

```powershell
$env:PYTHONPATH = 'src'
python -m ledgermatch examples/invoices.csv
```

[DEMO] Ejecuta el comando y localiza las líneas `Coinciden` y `Con diferencia`.

## Código real

Abre `src/ledgermatch/__main__.py`. No necesitas comprenderlo completo todavía. Identifica tres cosas:

- `import` trae capacidades de otros módulos;
- `main()` concentra el flujo principal;
- `print(...)` hace visible el resultado.

Después abre `examples/invoices.csv`. Ese archivo es la entrada real del programa.

## Qué acaba de pasar

El programa leyó cuatro filas válidas. Dos tienen el mismo importe facturado y pagado. Dos presentan diferencia. La aplicación no “entiende contabilidad”: ejecuta reglas explícitas que iremos descubriendo y mejorando.

Ya tienes un programa con entrada, procesamiento y salida.

## Errores comunes

- Ejecutar desde otra carpeta y obtener un error de ruta.
- Olvidar `PYTHONPATH=src` en esta etapa temprana.
- Cambiar comas o nombres de columnas del CSV sin comprender todavía el contrato.
- Usar una versión preview de Python sólo porque su número es mayor.

## Buenas prácticas

Empieza por una ejecución reproducible. Cuando algo falle, conserva el comando y el mensaje exacto. No arregles problemas cambiando cosas al azar.

## Tu turno

Copia `examples/invoices.csv` como `examples/mi-prueba.csv` y cambia el pago de `F-1001` de `1250.00` a `1200.00`.

Ejecuta LedgerMatch con tu archivo.

## Cómo comprobar

Antes del cambio el ejemplo contiene dos diferencias. Después debe contener tres. Además debe aparecer una línea para `F-1001` con diferencia negativa.

## Solución

La solución no requiere tocar Python. El objetivo es demostrar que ya distingues **código** de **datos de entrada** y sabes ejecutar la aplicación contra otro archivo.

## Reto adicional

¿Qué ocurre si escribes una ruta a un archivo inexistente? Lee el mensaje. Todavía no estudiaremos excepciones, pero observa que LedgerMatch convierte ese problema en una salida entendible.

## Resumen

- Python puede ejecutar un paquete con `python -m`.
- LedgerMatch ya resuelve un problema pequeño y verificable.
- Los datos de entrada viven fuera del código.
- La salida cambia cuando cambia la evidencia del CSV.

## Siguiente paso

En la [Lección 2](02-datos-y-tipos.md) vas a entender cómo representamos una factura, una fecha y, especialmente, dinero sin recurrir a aproximaciones binarias innecesarias.

## Referencias

- [Tutorial oficial de Python](https://docs.python.org/3.14/tutorial/)
- [Uso de Python](https://docs.python.org/3.14/using/)

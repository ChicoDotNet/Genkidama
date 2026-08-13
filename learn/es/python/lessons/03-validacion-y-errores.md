# Lección 3 — Validación y errores de entrada

## Qué vas a conseguir

Vas a seguir una fila desde el CSV hasta una de dos salidas: un `InvoiceRecord` utilizable o uno o más `ValidationIssue` que expliquen por qué no puede conciliarse.

## Antes de empezar

Abre `src/ledgermatch/parser.py` y localiza `read_invoices`.

## El problema

Los datos externos no respetan nuestros type hints. Una fecha puede venir vacía, un total puede decir `N/A` y una columna puede desaparecer de la exportación.

Una herramienta profesional inicial no necesita recuperarse de todo, pero sí debe distinguir **datos defectuosos** de **bugs del programa**.

## Concepto

LedgerMatch usa dos estrategias diferentes:

1. una estructura de archivo incompatible produce `CsvSchemaError` y detiene la operación;
2. una fila defectuosa produce `ValidationIssue`, se omite de la conciliación y permite revisar otras filas.

No todos los errores merecen la misma reacción.

## Demostración

Crea `examples/invalido.csv`:

```csv
invoice_id,customer,issued_on,invoice_total,payment_total
F-X,Cliente,2026-99-99,no-es-dinero,100.00
```

Ejecuta LedgerMatch con ese archivo.

[DEMO] Observa que no aparece un traceback. La salida identifica fila y campo.

## Código real

La función `_money` devuelve una pareja:

```python
(value, issue)
```

Si puede interpretar el importe, `issue` es `None`. Si no, el valor es `None` y existe una explicación estructurada.

Para fechas usamos `date.fromisoformat(...)` dentro de `try/except ValueError`. No atrapamos `Exception` de forma indiscriminada: sólo convertimos en validación el error que realmente esperamos de esa operación.

## Qué acaba de pasar

La aplicación mantiene una frontera. Dentro de la conciliación podemos confiar en que `InvoiceRecord` contiene fecha e importes utilizables porque la incertidumbre del archivo se resolvió antes.

Eso hace más pequeño el código posterior.

## Errores comunes

- Atrapar `Exception` y esconder bugs reales.
- Devolver sólo `False` sin explicar qué campo falló.
- Imprimir errores desde cada función y acoplar lógica a consola.
- Continuar con valores inventados como cero cuando falta dinero.
- Confiar en hints como sustituto de validación externa.

## Buenas prácticas

Valida en la frontera y conserva contexto. Un mensaje `valor inválido` es menos útil que `fila 2 / invoice_total: El importe debe ser decimal`.

## Tu turno

Construye un CSV con tres filas:

- una correcta;
- una sin cliente;
- una con importe negativo.

Ejecuta LedgerMatch.

## Cómo comprobar

La salida debe indicar una fila procesada y dos filas inválidas. Cada defecto debe señalar su campo. La fila correcta debe seguir conciliándose.

## Solución

No modifiques el código para conseguirlo: el parser actual ya diferencia esos casos. Si tu resultado no coincide, revisa los encabezados y el número de fila que muestra el programa.

## Reto adicional

Elimina la columna `payment_total` del encabezado. Ahora el problema ya no es una fila: el archivo completo incumple el esquema. Comprueba que LedgerMatch lo informa como `CSV inválido` y termina con código de salida 2.

## Resumen

- Datos inválidos y bugs no son lo mismo.
- Podemos acumular problemas de filas sin perder todo el archivo.
- Las excepciones son útiles cuando representan una condición excepcional bien definida.
- La validación temprana simplifica el dominio.

## Siguiente paso

En la [Lección 4](04-pruebas-y-checkpoint.md) convertirás estos comportamientos en pruebas ejecutables para que una mejora futura no los rompa accidentalmente.

## Referencias

- [Errores y excepciones](https://docs.python.org/3.14/tutorial/errors.html)
- [`csv.DictReader`](https://docs.python.org/3.14/library/csv.html#csv.DictReader)

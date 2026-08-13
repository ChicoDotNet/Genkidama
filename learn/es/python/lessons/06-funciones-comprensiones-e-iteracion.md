# Lección 6 — Funciones, comprehensions e iteración

## Qué vas a conseguir

Vas a convertir preguntas de usuario en funciones puras: seleccionar un cliente, mostrar sólo diferencias y combinar ambos criterios sin duplicar bucles.

## Antes de empezar

Abre `analytics.py` y ubica `select_lines` junto a `summarize_by_customer`.

## El problema

Si cada reporte implementa su propio `for` para filtrar líneas, pronto tendremos varias definiciones de “sólo diferencias” o de cómo comparar clientes.

## Concepto

Una función tiene entradas, una responsabilidad y un resultado. Los parámetros keyword-only ayudan a que una llamada con varios booleanos siga siendo legible:

```python
select_lines(summary, customer="Acme", only_differences=True)
```

La comprehension de la implementación conserva sólo las líneas que cumplen los criterios.

## Demostración

```python
return tuple(
    line
    for line in summary.lines
    if (customer_key is None or line.record.customer.casefold() == customer_key)
    and (not only_differences or line.status is MatchStatus.DIFFERENCE)
)
```

Léela como una frase: “para cada línea, consérvala si coincide el cliente y, cuando se pidió, si tiene diferencia”.

## Código real

El filtro devuelve una `tuple` nueva. No modifica `summary.lines`, no imprime y no depende de variables globales. Eso vuelve barato probar varias combinaciones.

## Qué acaba de pasar

Iterar no siempre significa escribir un bloque `for`. Un `for` explícito es mejor cuando hay varios pasos o efectos; una comprehension es útil cuando expresa una transformación corta y clara.

## Errores comunes

- anidar comprehensions hasta volverlas un acertijo;
- usar argumentos posicionales como `select_lines(x, None, True)` que esconden significado;
- cambiar la colección original mientras la recorres;
- meter lectura de archivos dentro de una función de selección.

## Buenas prácticas

Prefiere funciones pequeñas por comportamiento, nombres que permitan leer la llamada y datos inmutables en fronteras internas cuando no necesitas mutar.

## Tu turno

Escribe una prueba que pida `customer="acme"` y `only_differences=True`. Debe regresar sólo la factura con diferencia aunque el CSV haya escrito `Acme` con otra capitalización.

## Cómo comprobar

```bash
python -m pytest app/tests/test_analytics.py -v
```

## Solución

Compara tu expectativa con `test_select_lines_combines_customer_and_difference_filters`.

## Reto adicional

Implementa en una copia temporal el mismo filtro con un `for` y `append`. Decide cuál versión comunica mejor la regla y explica por qué.

## Resumen

- una función evita repetir una decisión;
- keyword-only hace explícita la intención;
- comprehensions sirven para transformaciones legibles;
- la lógica pura es fácil de probar porque no depende del entorno.

## Siguiente paso

En la [Lección 7](07-cli-rutas-y-archivos.md) esos filtros llegarán a una interfaz real mediante `argparse` y `pathlib`.

## Referencias

- [Definir funciones](https://docs.python.org/3.14/tutorial/controlflow.html#defining-functions)
- [List comprehensions](https://docs.python.org/3.14/tutorial/datastructures.html#list-comprehensions)

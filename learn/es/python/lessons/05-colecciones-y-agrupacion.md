# Lección 5 — Colecciones, agrupación y duplicados

## Qué vas a conseguir

Vas a reconocer cuándo usar lista, tupla, `set` y `dict` mientras LedgerMatch pasa de conciliar filas a responder una pregunta real: **¿qué está ocurriendo por cliente?**

## Antes de empezar

Ejecuta `python -m pytest` y confirma que el checkpoint anterior dejó una regresión para facturas duplicadas.

## El problema

Una lista de 500 diferencias ayuda poco si dirección quiere saber qué clientes concentran el desfase. Necesitamos agrupar líneas aceptadas sin perder precisión ni mezclar esa responsabilidad con leer CSV.

## Concepto

Python tiene colecciones con intenciones distintas:

- `list`: secuencia mutable;
- `tuple`: secuencia que entregamos como resultado estable;
- `set`: pertenencia sin duplicados, como los `invoice_id` ya vistos;
- `dict`: relaciona una clave con un valor, ideal para construir grupos.

El checkpoint 01 ya justificó un `set[str]`: preguntar si un identificador apareció antes. Ahora `analytics.py` usa un `dict` para reunir líneas por cliente.

## Demostración

[EN PANTALLA]

```python
buckets: dict[str, list[ReconciliationLine]] = {}

for line in summary.lines:
    key = line.record.customer.casefold()
    buckets.setdefault(key, []).append(line)
```

`casefold()` permite que `Acme` y `ACME` compartan grupo sin modificar el nombre original que mostramos.

## Código real

`summarize_by_customer` produce `CustomerSummary` con conteo, total facturado, total pagado y diferencia. Los importes siguen siendo `Decimal`.

La función no abre archivos, no imprime y no conoce argumentos de terminal. Recibe datos y devuelve datos.

## Qué acaba de pasar

La misma aplicación ya usa cuatro colecciones por razones visibles. No elegimos estructuras por costumbre: elegimos la operación que necesitamos hacer sobre ellas.

## Errores comunes

- usar una lista para buscar repetidamente elementos únicos;
- asumir que un `dict` conserva por sí solo el orden de negocio que deseas comunicar;
- agrupar dinero usando `float` sólo porque el código es corto;
- normalizar un nombre destruyendo el valor que luego quieres mostrar.

## Buenas prácticas

Haz explícita la clave de agrupación, conserva resultados deterministas y entrega colecciones que no permitan mutaciones accidentales cuando el consumidor sólo debe leer.

## Tu turno

Agrega temporalmente una tercera factura de un cliente existente y usa `summarize_by_customer` desde una prueba. Comprueba a mano conteo y diferencia antes de escribir el `assert`.

## Cómo comprobar

```bash
python -m pytest app/tests/test_analytics.py -v
```

## Solución

La prueba del repositorio muestra una agrupación que combina `Acme` y `ACME` y conserva el primer nombre como presentación.

## Reto adicional

Explica por qué la clave normalizada pertenece al cálculo y no necesariamente debe reemplazar el nombre original del cliente.

## Resumen

- `set` resuelve pertenencia sin duplicados;
- `dict` construye agrupaciones;
- una `tuple` comunica un resultado estable;
- las colecciones deben responder al problema, no a una preferencia personal.

## Siguiente paso

En la [Lección 6](06-funciones-comprensiones-e-iteracion.md) convertirás filtros repetibles en funciones pequeñas y leerás comprehensions como transformaciones, no como trucos de sintaxis.

## Referencias

- [Estructuras de datos](https://docs.python.org/3.14/tutorial/datastructures.html)

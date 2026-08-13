# Lección 9 — Persistencia local con SQLite

## Qué vas a conseguir

LedgerMatch dejará de olvidar cada conciliación al terminar el proceso. Vas a guardar un resumen y sus líneas aceptadas en una base SQLite local, sin instalar un servidor ni añadir un ORM.

## Antes de empezar

Desde `learn/es/python/app` ejecuta:

```bash
python -m pytest
```

Después procesa el archivo de ejemplo una vez:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --db ledgermatch.db
```

## El problema

Hasta ahora el resultado existe sólo mientras el proceso está vivo. Puedes imprimirlo o exportarlo, pero no responder después preguntas tan básicas como:

- ¿qué archivo concilié ayer?;
- ¿cuánto facturado tenía esa corrida?;
- ¿cuántas filas inválidas detectó?;
- ¿qué diferencias pertenecían a ese procesamiento?

Necesitamos estado durable.

## Concepto: una frontera de persistencia

Python incluye `sqlite3` en la biblioteca estándar. SQLite guarda tablas e índices dentro de un archivo y es suficiente para este problema local.

Abre [`storage.py`](../app/src/ledgermatch/storage.py). La clase `SqliteRunRepository` concentra la responsabilidad de hablar SQL. El parser no conoce SQLite y el reconciliador tampoco.

La base guarda dos conjuntos:

- `reconciliation_runs`: una fila por importación;
- `reconciliation_lines`: las líneas aceptadas que pertenecen a esa importación.

## Demostración

[EJECUTAR]

Borra una base de prueba si existe y ejecuta:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --db demo.db
```

Después vuelve a ejecutar exactamente el mismo comando.

La base `demo.db` sobrevive a ambos procesos. En la siguiente lección veremos por qué la segunda ejecución no crea una copia duplicada.

## Código real

En `save_run` verás parámetros SQL:

```python
connection.execute(
    """
    INSERT INTO reconciliation_runs(
        source_name, source_sha256, imported_at, line_count,
        invalid_rows, invoice_total, payment_total
    )
    VALUES (?, ?, ?, ?, ?, ?, ?)
    """,
    (...),
)
```

Los `?` son placeholders. Los valores viajan aparte del texto SQL. No concatenamos el nombre del cliente, el identificador de factura ni importes dentro de la sentencia.

Los importes se guardan como texto porque el modelo de negocio ya eligió `Decimal`; convertirlos silenciosamente a `float` sólo para persistir perdería esa decisión.

## Qué acaba de pasar

LedgerMatch ganó una capacidad de infraestructura sin contaminar la regla de conciliación. `reconcile()` sigue siendo una función pura: recibe un `ParseResult` y devuelve un `ReconciliationSummary`.

La persistencia ocurre después.

## Errores comunes

### Guardar todo en una sola tabla gigante

Puede funcionar al principio, pero repite metadata de la importación en cada línea y vuelve ambiguas las relaciones.

### Concatenar SQL

```python
f"INSERT ... VALUES ('{customer}')"
```

mezcla datos con código SQL. Usa parámetros.

### Cambiar `Decimal` por `float` porque SQLite “no tiene Decimal”

La frontera de almacenamiento debe representar la decisión del dominio, no borrar su intención.

### Abrir una conexión global eterna

Para esta aplicación local preferimos conexiones cortas y explícitas. Facilitan pruebas y evitan estado oculto.

## Buenas prácticas

- centraliza SQL en una frontera clara;
- parametriza valores;
- activa claves foráneas cuando dependes de ellas;
- prueba cerrando una instancia y abriendo otra;
- conserva tipos de negocio deliberadamente.

## Tu turno

Agrega una prueba que:

1. cree una base dentro de `tmp_path`;
2. guarde una corrida;
3. cree otra instancia de `SqliteRunRepository`;
4. llame `list_runs()`;
5. compruebe que totales e identificador sobrevivieron.

No pruebes sólo que “el archivo existe”. Prueba el comportamiento que necesitas recuperar.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

```bash
python -m pytest tests/test_storage.py -v
```

## Solución

La suite del proyecto contiene una prueba equivalente en `test_storage.py`. Léela después de intentar tu propia versión.

## Reto adicional

¿Por qué `reconciliation_lines.run_id` tiene una clave foránea? ¿Qué inconsistencia podría aparecer si aceptáramos líneas sin una corrida padre?

## Resumen

- `sqlite3` permite persistencia relacional local sin un servidor;
- un repositorio concentra SQL y recursos;
- los parámetros separan datos de sentencias;
- `Decimal` sigue siendo una decisión del modelo aunque el almacenamiento use texto;
- persistir se demuestra reabriendo el almacenamiento.

## Siguiente paso

En la [Lección 10](10-transacciones-e-idempotencia.md) haremos que guardar una corrida sea una unidad atómica y que procesar dos veces el mismo archivo no duplique el historial.

## Referencias

- [`sqlite3`](https://docs.python.org/3.14/library/sqlite3.html)
- [Placeholders en `sqlite3`](https://docs.python.org/3.14/library/sqlite3.html#how-to-use-placeholders-to-bind-values-in-sql-queries)

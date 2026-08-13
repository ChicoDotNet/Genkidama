# Lección 10 — Transacciones e idempotencia

## Qué vas a conseguir

Vas a proteger dos propiedades profesionales: una importación se guarda completa o no se guarda, y ejecutar dos veces el mismo archivo no crea dos corridas idénticas.

## El problema

Guardar una corrida requiere varias escrituras:

1. crear la fila de `reconciliation_runs`;
2. insertar cero o más `reconciliation_lines`.

Si la tercera línea falla y las anteriores ya quedaron confirmadas, el historial queda incompleto.

Existe otro problema operativo: un usuario puede ejecutar dos veces el mismo CSV por accidente. Duplicar el historial no agrega información; sólo agrega confusión.

## Concepto: transacción

Una transacción agrupa cambios de forma que se confirmen juntos con `commit()` o se deshagan con `rollback()`.

`SqliteRunRepository.save_run()` abre la conexión con:

```python
sqlite3.connect(self._database, autocommit=False)
```

El código sólo hace `commit()` después de insertar la corrida y todas sus líneas. Si ocurre una excepción, hace `rollback()`.

## Concepto: idempotencia

Una operación idempotente puede repetirse sin producir un efecto acumulativo incorrecto.

LedgerMatch calcula SHA-256 sobre el contenido del archivo:

```python
fingerprint = sha256_file(source)
```

La columna `source_sha256` es `UNIQUE`. El `INSERT` usa:

```sql
ON CONFLICT(source_sha256) DO NOTHING
```

Si ya existe esa huella, el repositorio recupera el `run_id` previo y devuelve:

```python
SaveResult(run_id=..., created=False)
```

La aplicación puede informar “ya registrada” sin convertir una repetición accidental en un fallo.

## Demostración

[EJECUTAR]

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --db demo.db
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --db demo.db
```

La primera ejecución debe indicar `creada`; la segunda, `ya registrada`, usando el mismo identificador.

## Código real

`sha256_file()` lee bloques de 64 KiB:

```python
for chunk in iter(lambda: stream.read(64 * 1024), b""):
    digest.update(chunk)
```

No necesita cargar un archivo entero en memoria para calcular su huella.

La huella identifica el **contenido**, no el nombre. Renombrar exactamente el mismo archivo no crea una conciliación nueva.

## Qué acaba de pasar

Ahora existen dos invariantes:

- no hay una corrida parcialmente guardada;
- un mismo contenido no genera copias repetidas.

Son propiedades distintas. Una transacción no resuelve por sí sola idempotencia y una restricción `UNIQUE` no reemplaza una transacción.

## Errores comunes

### Usar el nombre del archivo como identificador único

Dos archivos diferentes pueden llamarse `facturas.csv`. El nombre no representa su contenido.

### Hacer `commit()` después de cada línea

Pierdes atomicidad.

### Capturar cualquier excepción y fingir éxito

Un rollback protege los datos; no significa que debas ocultar el fallo que lo provocó.

### Crear un hash con `hash()`

El `hash()` integrado no es una huella persistente de archivos. Para este caso usamos `hashlib.sha256`.

## Buenas prácticas

- define qué operación debe ser atómica;
- deja la unicidad donde también pueda protegerla la base;
- devuelve un resultado que distinga “creado” de “ya existía”;
- conserva el fingerprint como dato auditable;
- prueba la segunda ejecución, no sólo la primera.

## Tu turno

Escribe una prueba que guarde dos veces la misma corrida y compruebe:

- `first.created is True`;
- `second.created is False`;
- ambos `run_id` son iguales;
- `list_runs()` contiene una sola corrida.

## Cómo comprobar

```bash
python -m pytest tests/test_storage.py -v
```

## Solución

La prueba de idempotencia incluida en `test_storage.py` muestra una referencia. Primero intenta escribir la tuya leyendo sólo el contrato público.

## Reto adicional

¿Qué debería ocurrir si cambia un solo centavo en el CSV? Explica por qué la huella debe cambiar y por qué esa corrida sí representa información nueva.

## Resumen

- una transacción conserva el “todo o nada”;
- `rollback()` es parte del manejo de fallos;
- SHA-256 permite identificar contenido;
- `UNIQUE` + resultado explícito hacen idempotente la importación;
- idempotencia evita deuda de datos por reintentos normales.

## Siguiente paso

En la [Lección 11](11-reportes-csv-json.md) convertiremos el mismo resultado en archivos JSON y CSV útiles para otras herramientas, sin duplicar la lógica de conciliación.

## Referencias

- [Control de transacciones con `sqlite3`](https://docs.python.org/3.14/library/sqlite3.html#transaction-control)
- [`hashlib`](https://docs.python.org/3.14/library/hashlib.html)

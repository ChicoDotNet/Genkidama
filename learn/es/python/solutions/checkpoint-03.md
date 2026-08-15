# Solución de referencia — Checkpoint 03

Una solución pequeña es construir dos consultas explícitas y mantener los parámetros separados del SQL.

```python
def list_runs(self, source_name: str | None = None) -> tuple[StoredRun, ...]:
    if not self._database.exists():
        return ()

    connection = sqlite3.connect(self._database)
    try:
        self._ensure_schema(connection)
        if source_name is None:
            rows = connection.execute(
                """
                SELECT id, source_name, source_sha256, imported_at, line_count,
                       invalid_rows, invoice_total, payment_total
                FROM reconciliation_runs
                ORDER BY id DESC
                """
            ).fetchall()
        else:
            rows = connection.execute(
                """
                SELECT id, source_name, source_sha256, imported_at, line_count,
                       invalid_rows, invoice_total, payment_total
                FROM reconciliation_runs
                WHERE source_name = ?
                ORDER BY id DESC
                """,
                (source_name,),
            ).fetchall()
        ...
    finally:
        connection.close()
```

Esta referencia elige comparación **exacta** porque el valor persistido es metadata del archivo, no un identificador de cliente normalizado. Otra decisión puede ser válida si se documenta y prueba.

Prueba representativa:

```python
assert tuple(run.source_name for run in repository.list_runs("enero.csv")) == (
    "enero.csv",
)
```

Lo importante es que:

- no cambie el comportamiento sin filtro;
- el filtro llegue parametrizado a SQLite;
- el orden siga explícito;
- el contrato quede documentado y protegido por pruebas.

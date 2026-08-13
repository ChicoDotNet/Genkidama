# Checkpoint 03 — Filtra el historial por archivo fuente

Un operador quiere consultar únicamente las corridas registradas para un nombre de archivo determinado.

## Trabajo

Extiende `SqliteRunRepository.list_runs()` para aceptar un filtro opcional:

```python
list_runs(source_name: str | None = None)
```

Criterios:

- sin argumento, conserva exactamente el comportamiento actual;
- con `source_name`, devuelve sólo corridas cuyo nombre fuente coincida;
- la consulta debe seguir parametrizada;
- el orden debe continuar siendo del `run_id` más reciente al más antiguo;
- documenta si la comparación es exacta o normalizada;
- agrega pruebas para el camino sin filtro y con filtro.

No cargues todas las corridas para filtrarlas después en Python si SQLite puede resolver el criterio directamente.

## Cómo comprobar

```bash
python -m pytest
```

La suite existente debe seguir verde y tus pruebas nuevas deben demostrar ambos caminos.

Después compara con [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).

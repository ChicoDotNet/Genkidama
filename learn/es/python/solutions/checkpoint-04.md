# Solución de referencia — Checkpoint 04

Una solución pequeña resuelve rutas antes de efectos:

```python
def validate_report_destinations(source, destinations):
    source_path = Path(source).resolve()
    seen = set()
    for raw_destination in destinations:
        destination = Path(raw_destination).resolve()
        if destination == source_path:
            raise OutputPolicyError("Un reporte no puede usar la misma ruta que el CSV de entrada.")
        if destination in seen:
            raise OutputPolicyError("Los reportes JSON y CSV no pueden usar la misma ruta.")
        seen.add(destination)
```

La CLI ejecuta esta validación antes de crear/importar mediante el repositorio. Prueba que el error devuelve 2, preserva el source y no crea SQLite.

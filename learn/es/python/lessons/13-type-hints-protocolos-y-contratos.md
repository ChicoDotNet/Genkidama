# Lección 13 — Type hints, Protocol y contratos internos

## Qué vas a conseguir

Vas a hacer explícito qué necesita el servicio de importación de cualquier repositorio compatible, sin convertir el diseño en una jerarquía de clases innecesaria.

Al terminar podrás leer type hints como documentación para herramientas y usar `Protocol` para describir una capacidad estructural.

## El problema

`import_csv()` necesita guardar una conciliación. Si su firma depende directamente de `SqliteRunRepository`, parece que SQLite forma parte de la regla del caso de uso.

Pero el servicio realmente necesita algo más pequeño: un objeto que pueda ejecutar `save_run(...)` y devolver `SaveResult`.

## Concepto

Una anotación como `def sha256_file(path: str | Path) -> str` comunica qué acepta y devuelve una función. Python no convierte automáticamente esas anotaciones en validación de runtime.

En [`ports.py`](../app/src/ledgermatch/ports.py), `RunRepository(Protocol)` describe estructuralmente la operación que necesita el servicio. No exigimos que una implementación herede de esa clase.

## Demostración

Abre `tests/test_ports.py`: utiliza un repositorio mínimo en memoria que no hereda de SQLite.

```bash
python -m pytest tests/test_ports.py -v
```

## Qué acaba de pasar

La frontera es explícita porque existe una razón real de cambio: almacenamiento. El servicio queda fácil de probar sin inventar una interfaz para cada clase.

## Errores comunes

- pensar que un type hint valida strings vacíos;
- crear `Protocol` para cada función;
- hacer que dominio importe CLI;
- confundir flexibilidad estructural con ausencia de contrato.

## Buenas prácticas

- anota superficies públicas;
- usa protocolos en fronteras reales;
- conserva lógica determinista separada del I/O;
- prueba sustitutos pequeños cuando demuestran desacoplamiento útil.

## Tu turno

Crea otro repositorio falso que cuente llamadas a `save_run()`. Procesa un CSV válido y comprueba que recibe el resumen una sola vez y que `import_csv()` conserva su `SaveResult`.

## Cómo comprobar

```bash
python -m pytest tests/test_ports.py -v
python -m pytest
```

## Reflexión

¿Qué ganamos usando `RunRepository` si producción sigue usando SQLite? Explica la respuesta en términos de frontera, pruebas y razones de cambio.

## Resumen

- type hints explicitan intención, no validación automática;
- `Protocol` expresa typing estructural;
- una frontera de almacenamiento justifica una abstracción pequeña;
- el servicio no necesita conocer SQLite.

## Siguiente paso

En la [Lección 14](14-entornos-dependencias-y-empaquetado.md) convertiremos la carpeta en un paquete instalable con `pyproject.toml` y el comando `ledgermatch`.

## Referencias

- [Typing — Python 3.14](https://docs.python.org/3.14/library/typing.html)
- [Protocols](https://typing.python.org/en/latest/spec/protocol.html)

# Lección 14 — Entornos, dependencias y empaquetado moderno

## Qué vas a conseguir

Vas a instalar LedgerMatch como paquete real y ejecutar `ledgermatch` sin depender de `PYTHONPATH`.

## El problema

`PYTHONPATH=src python -m ledgermatch` sirve durante construcción, pero no es una entrega cómoda ni declara dependencias de forma reproducible.

## Concepto: `pyproject.toml`

[`pyproject.toml`](../app/pyproject.toml) declara build backend, metadata, versión de Python, extras y entry point.

```toml
[build-system]
requires = ["setuptools==83.0.0"]
build-backend = "setuptools.build_meta"
```

El proyecto declara cero dependencias de runtime y pytest en el extra `dev`.

## Demostración

Desde un entorno virtual limpio:

```bash
python -m pip install -e ".[dev]"
python -m pytest
ledgermatch examples/invoices.csv --db demo.db
python -m pip wheel --no-deps . -w dist
```

El CI ejecuta esta ruta con Python 3.14.7.

## Qué acaba de pasar

Ahora existen un contrato de instalación, un comando de consola y un artefacto wheel verificable.

## Errores comunes

- instalar globalmente;
- poner pytest como dependencia de runtime;
- tratar `pip freeze` como diseño de dependencias;
- actualizar versiones sin verificar.

## Buenas prácticas

- usa `venv`;
- centraliza metadata en `pyproject.toml`;
- separa runtime y desarrollo;
- prueba clean install y wheel.

## Tu turno

Crea un entorno nuevo, instala `.[dev]`, ejecuta `ledgermatch --help`, pruebas y build del wheel. La evidencia importante es reconstruir desde limpio.

## Resumen

- `pyproject.toml` centraliza build/metadata;
- los extras separan tooling;
- un entry point vuelve instalable la herramienta;
- un wheel demuestra empaquetado.

## Siguiente paso

En la [Lección 15](15-debugging-profiling-y-diagnostico.md) usaremos debugger, profiling y logs antes de editar a ciegas.

## Referencias

- [Python Packaging User Guide](https://packaging.python.org/)
- [venv](https://docs.python.org/3.14/library/venv.html)

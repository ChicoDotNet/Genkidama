# Curso de Python desde cero — Construye un conciliador de facturas CSV

Este curso enseña Python desde cero construyendo **LedgerMatch**, una CLI que importa facturas/pagos desde CSV, valida, concilia, persiste corridas idempotentes y produce reportes reproducibles.

## ¿Qué es Python y para qué se utiliza?

Python es un lenguaje de propósito general usado en automatización, datos, backend, integraciones, tooling, ciencia de datos e IA. Aquí lo aplicamos a convertir archivos operativos en información verificable.

## ¿Puedo aprenderlo desde cero?

Sí. No necesitas experiencia previa programando. Necesitas Python, terminal y editor.

## ¿Qué vas a construir?

**LedgerMatch** valida dinero con `Decimal`, concilia diferencias, conserva historial SQLite y exporta JSON/CSV. La aplicación vive en [`app/`](app/).

## Tooling verificado

- CPython **3.14.7**;
- `venv` + `pip`;
- setuptools **83.0.0**;
- pytest **9.1.1**;
- SQLite estándar `sqlite3`;
- Windows 11/PowerShell/VS Code objetivo ideal y Linux/bash/VS Code alternativa.

## Instalación

Desde `app/`:

```bash
python -m venv .venv
python -m pip install -e ".[dev]"
ledgermatch --help
```

## Build

```bash
python -m pip wheel --no-deps . -w dist
```

## Run

```bash
ledgermatch examples/invoices.csv --db ledgermatch.db
ledgermatch examples/invoices.csv --only-differences
ledgermatch examples/invoices.csv --json report.json --csv report.csv
```

No sobrescribe reportes por default; `--force` autoriza únicamente reemplazo consciente de reportes. `--db`/`LEDGERMATCH_DB` configuran SQLite y `--max-input-bytes`/`LEDGERMATCH_MAX_INPUT_BYTES` configuran el límite de entrada.

## Test

```bash
python -m pytest
```

## Qué sabrás hacer

Tipos/datos, colecciones, archivos, errores, módulos, CLI, SQLite, SQL parametrizado, transacciones, idempotencia, reporting, type hints/Protocol, packaging, debugging/profiling, pruebas y hardening básico.

## Ruta — 17/17 implementadas

1. [Primera conciliación](lessons/01-tu-primera-conciliacion.md)
2. [Datos y tipos](lessons/02-datos-y-tipos.md)
3. [Validación y errores](lessons/03-validacion-y-errores.md)
4. [pytest y checkpoint 01](lessons/04-pruebas-y-checkpoint.md)
5. [Colecciones y agrupación](lessons/05-colecciones-y-agrupacion.md)
6. [Funciones, comprehensions e iteración](lessons/06-funciones-comprensiones-e-iteracion.md)
7. [CLI, rutas y archivos](lessons/07-cli-rutas-y-archivos.md)
8. [Módulos y checkpoint 02](lessons/08-modulos-responsabilidades-y-checkpoint.md)
9. [SQLite](lessons/09-persistencia-sqlite.md)
10. [Transacciones e idempotencia](lessons/10-transacciones-e-idempotencia.md)
11. [Reportes CSV/JSON](lessons/11-reportes-csv-json.md)
12. [Configuración, logging y checkpoint 03](lessons/12-configuracion-logging-y-checkpoint.md)
13. [Type hints, Protocol y contratos](lessons/13-type-hints-protocolos-y-contratos.md)
14. [Entornos, dependencias y empaquetado](lessons/14-entornos-dependencias-y-empaquetado.md)
15. [Debugging, profiling y diagnóstico](lessons/15-debugging-profiling-y-diagnostico.md)
16. [Seguridad de entradas y hardening + checkpoint 04](lessons/16-seguridad-de-entradas-y-hardening.md)
17. [Evaluación final sin receta](lessons/17-evaluacion-final.md)

## Checkpoints y evaluación

- [Checkpoint 01](exercises/checkpoint-01.md) / [solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) / [solución](solutions/checkpoint-02.md)
- [Checkpoint 03](exercises/checkpoint-03.md) / [solución](solutions/checkpoint-03.md)
- [Checkpoint 04](exercises/checkpoint-04.md) / [solución](solutions/checkpoint-04.md)
- [Evaluación final](exercises/evaluacion-final.md) / [rúbrica](exercises/rubrica-final.md) / [solución de referencia](solutions/evaluacion-final.md)

## Trabajo y alcance

Los fundamentos son transferibles a automatización, ETL ligero, scripts, integraciones, mantenimiento y backend Python. El curso produce evidencia práctica; **no garantiza contratación**.

## FAQ

### ¿Por qué no empezar con Django/FastAPI/pandas/IA?
Porque primero dominamos lenguaje, I/O, errores, persistencia, tooling y pruebas sin esconderlos detrás de frameworks.

### ¿Por qué `Decimal`?
Para representar dinero explícita y predeciblemente.

### ¿Por qué SQLite?
Permite aprender SQL/transacciones/idempotencia localmente sin administrar servidor.

### ¿Qué aporta `Protocol`?
Hace visible la capacidad que el servicio necesita del almacenamiento sin obligar herencia nominal.

### ¿Está listo para sistema financiero multiusuario?
No. Es una herramienta educativa local endurecida para su contexto; multiusuario exige decisiones adicionales de identidad, autorización, concurrencia, respaldo y operación.

## Glosario

`Decimal`: aritmética decimal; `dataclass`: tipo orientado a datos; `argparse`: CLI; transacción: cambios como unidad; idempotencia: repetición sin efecto acumulativo incorrecto; fingerprint: huella de contenido; `Protocol`: typing estructural; wheel: artefacto instalable; profiling: medición de tiempo; hardening: reducción deliberada de riesgo.

## Cómo hablar de este proyecto en una entrevista

Explica problema → decisiones → confiabilidad → operación → tradeoff: `Decimal`; fronteras parser/reconciler/service/storage/reporting; SQL parametrizado; transacción + SHA-256; configuración/logs/exportaciones/hardening; paquete wheel + CI. SQLite es correcto para esta herramienta local, no una promesa automática de arquitectura distribuida.

## Referencias oficiales

- [Python 3.14](https://docs.python.org/3.14/)
- [`decimal`](https://docs.python.org/3.14/library/decimal.html)
- [`sqlite3`](https://docs.python.org/3.14/library/sqlite3.html)
- [`typing`](https://docs.python.org/3.14/library/typing.html)
- [`logging`](https://docs.python.org/3.14/library/logging.html)
- [`pdb`](https://docs.python.org/3.14/library/pdb.html)
- [`cProfile`](https://docs.python.org/3.14/library/profile.html)
- [Packaging User Guide](https://packaging.python.org/)
- [pytest](https://docs.pytest.org/)

## Siguiente paso

Empieza en la [Lección 1](lessons/01-tu-primera-conciliacion.md) o, si terminaste la ruta guiada, realiza la [evaluación final](exercises/evaluacion-final.md) sin abrir la solución.

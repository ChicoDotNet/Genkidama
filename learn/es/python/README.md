# Curso de Python desde cero — Construye un conciliador de facturas CSV

Este curso enseña Python desde cero construyendo **LedgerMatch**, una herramienta de línea de comandos que importa facturas y pagos desde CSV, valida entradas, detecta diferencias, persiste corridas de forma idempotente y produce reportes reproducibles.

No empieza con horas de sintaxis aislada: en la primera lección ejecutas una conciliación real.

## ¿Qué es Python y para qué se utiliza?

Python es un lenguaje de propósito general ampliamente usado para automatización, tratamiento de datos, backend, integraciones, tooling, ciencia de datos e IA. Aquí empezamos por una capacidad especialmente natural para el lenguaje: convertir archivos operativos en información verificable.

## ¿Puedo aprenderlo desde cero?

Sí. No necesitas experiencia previa programando. Sólo debes poder instalar Python, abrir una terminal y editar archivos en VS Code.

## ¿Qué vas a construir?

**LedgerMatch** recibe registros con importe facturado e importe pagado, valida datos con tipos adecuados para negocio, concilia diferencias y conserva el historial en SQLite. El mismo resultado puede verse en terminal o exportarse a JSON/CSV.

La aplicación vive en [`app/`](app/).

## Tooling verificado

- **CPython 3.14.7**;
- `venv` + `pip`;
- **pytest 9.1.1** para pruebas;
- SQLite mediante el módulo estándar `sqlite3`;
- Windows 11 + PowerShell + VS Code como entorno ideal;
- Linux actual + bash + VS Code como alternativa de primera clase.

Python 3.15 todavía no se usa en este curso porque continúa en pre-release al momento de esta verificación.

## Instalar

Comprueba que tienes Python 3.14:

```bash
python --version
```

En Windows también puedes usar:

```powershell
py -3.14 --version
```

## Preparar el entorno

Desde `learn/es/python/app`:

```bash
python -m venv .venv
```

Activa el entorno y, para ejecutar pruebas:

```bash
python -m pip install pytest==9.1.1
```

## Run

Linux/macOS:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --db ledgermatch.db
```

PowerShell:

```powershell
$env:PYTHONPATH = 'src'
python -m ledgermatch examples/invoices.csv --db ledgermatch.db
```

Filtros y exportaciones:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --only-differences
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --customer "Cliente Uno" --by-customer
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --json report.json --csv report.csv
```

La ruta SQLite puede venir de `--db`, de `LEDGERMATCH_DB` o del default local `ledgermatch.db`.

## Test

```bash
python -m pytest
```

## Qué sabrás hacer al terminar

La meta es que puedas leer y escribir Python sencillo e idiomático; modelar datos; trabajar con colecciones, archivos y excepciones; separar responsabilidades; usar módulos y paquetes; persistir con transacciones; diseñar reintentos idempotentes; producir reportes; diagnosticar fallos; consultar documentación oficial y modificar una base existente sin receta.

## Ruta del curso

Estado actual: **12 de 17 lecciones implementadas**.

1. [Tu primera conciliación en ejecución](lessons/01-tu-primera-conciliacion.md)
2. [Datos de negocio: strings, fechas, Decimal y dataclasses](lessons/02-datos-y-tipos.md)
3. [Validación y errores de entrada](lessons/03-validacion-y-errores.md)
4. [Pruebas con pytest y primer checkpoint](lessons/04-pruebas-y-checkpoint.md)
5. [Colecciones, agrupación y duplicados](lessons/05-colecciones-y-agrupacion.md)
6. [Funciones, comprehensions e iteración](lessons/06-funciones-comprensiones-e-iteracion.md)
7. [Argumentos de CLI, rutas y archivos](lessons/07-cli-rutas-y-archivos.md)
8. [Módulos, responsabilidades y checkpoint 02](lessons/08-modulos-responsabilidades-y-checkpoint.md)
9. [Persistencia local con SQLite](lessons/09-persistencia-sqlite.md)
10. [Transacciones e idempotencia](lessons/10-transacciones-e-idempotencia.md)
11. [Reportes CSV y JSON](lessons/11-reportes-csv-json.md)
12. [Configuración, logging y checkpoint 03](lessons/12-configuracion-logging-y-checkpoint.md)
13. Type hints, protocolos y contratos internos
14. Entornos, dependencias y empaquetado moderno
15. Debugging, profiling y diagnóstico
16. Seguridad de entradas y hardening + checkpoint 04
17. Evaluación final sin receta

## Checkpoints

- después de la lección 4: [`checkpoint-01`](exercises/checkpoint-01.md), con [`solución de referencia`](solutions/checkpoint-01.md);
- después de la lección 8: [`checkpoint-02`](exercises/checkpoint-02.md), con [`solución de referencia`](solutions/checkpoint-02.md);
- después de la lección 12: [`checkpoint-03`](exercises/checkpoint-03.md), con [`solución de referencia`](solutions/checkpoint-03.md).

## ¿Qué tipo de trabajo utiliza estas habilidades?

Son fundamentos transferibles a automatización, ETL ligero, scripts operativos, integraciones, mantenimiento de aplicaciones Python y backend. El curso busca evidencia práctica; no garantiza contratación ni sustituye experiencia real en equipos.

## Preguntas frecuentes

### ¿Por qué no empezamos con Django, FastAPI, pandas o IA?

Porque antes de depender de frameworks conviene dominar el lenguaje, archivos, tipos, errores, módulos, persistencia y pruebas. LedgerMatch resuelve un problema real sin esconder esos fundamentos.

### ¿Por qué `Decimal` y no `float` para dinero?

Porque una conciliación financiera necesita representar decimales de forma explícita y predecible.

### ¿Por qué SQLite?

Porque permite aprender persistencia, SQL, transacciones e idempotencia con un archivo local y sin administrar un servidor.

### ¿Por qué SHA-256 si no estamos guardando contraseñas?

Aquí no se usa como contraseña: funciona como fingerprint estable del contenido para reconocer una importación repetida.

### ¿Tengo que aprender Git aquí?

No. Git tendrá su propio curso.

## Glosario

- **Intérprete:** programa que ejecuta código Python.
- **Módulo:** archivo Python que puede importarse.
- **Paquete:** conjunto organizado de módulos.
- **CSV:** formato tabular de texto separado por delimitadores.
- **Decimal:** tipo para aritmética decimal exacta en casos como dinero.
- **dataclass:** forma concisa de declarar clases orientadas principalmente a datos.
- **pytest:** herramienta de pruebas utilizada por el curso.
- **comprehension:** sintaxis compacta para construir o seleccionar elementos.
- **argparse:** módulo estándar para interfaces de línea de comandos.
- **transacción:** conjunto de cambios que se confirma o revierte como unidad.
- **idempotencia:** propiedad que permite repetir una operación sin acumular efectos incorrectos.
- **fingerprint:** huella estable derivada del contenido.
- **logging:** registro operacional separado de la salida de negocio.

## Cómo hablar de este proyecto en una entrevista

Explica el problema antes que las bibliotecas: recibiste datos tabulares de facturación/pagos, modelaste dinero con `Decimal`, separaste parsing de conciliación y analytics, convertiste entradas defectuosas en errores controlados, construiste una CLI, persististe corridas en SQLite con SQL parametrizado, protegiste atomicidad con transacciones, hiciste idempotentes los reintentos mediante SHA-256 y generaste salidas de terminal, JSON y CSV con pruebas automatizadas.

## Referencias oficiales

- [Documentación de Python 3.14](https://docs.python.org/3.14/)
- [`csv`](https://docs.python.org/3.14/library/csv.html)
- [`decimal`](https://docs.python.org/3.14/library/decimal.html)
- [`dataclasses`](https://docs.python.org/3.14/library/dataclasses.html)
- [`argparse`](https://docs.python.org/3.14/library/argparse.html)
- [`pathlib`](https://docs.python.org/3.14/library/pathlib.html)
- [`sqlite3`](https://docs.python.org/3.14/library/sqlite3.html)
- [`hashlib`](https://docs.python.org/3.14/library/hashlib.html)
- [`logging`](https://docs.python.org/3.14/library/logging.html)
- [`json`](https://docs.python.org/3.14/library/json.html)
- [Python Packaging User Guide](https://packaging.python.org/)
- [pytest](https://docs.pytest.org/)

## Siguiente paso

Si empiezas desde cero, ve a la [Lección 1](lessons/01-tu-primera-conciliacion.md). Si ya completaste el checkpoint 02, continúa con la [Lección 9](lessons/09-persistencia-sqlite.md). Si terminaste el checkpoint 03, el siguiente bloque empieza en la lección 13.

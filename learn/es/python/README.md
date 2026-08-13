# Curso de Python desde cero — Construye un conciliador de facturas CSV

Este curso enseña Python desde cero construyendo **LedgerMatch**, una herramienta de línea de comandos que importa datos de facturas y pagos desde CSV, valida entradas, detecta diferencias y crecerá hasta persistir resultados y producir reportes reproducibles.

No empieza con horas de sintaxis aislada: en la primera lección ejecutas una conciliación real.

## ¿Qué es Python y para qué se utiliza?

Python es un lenguaje de propósito general ampliamente usado para automatización, tratamiento de datos, backend, integraciones, tooling, ciencia de datos e IA. Aquí empezamos por una capacidad especialmente natural para el lenguaje: convertir archivos operativos en información verificable.

## ¿Puedo aprenderlo desde cero?

Sí. No necesitas experiencia previa programando. Sólo debes poder instalar Python, abrir una terminal y editar archivos en VS Code.

## ¿Qué vas a construir?

**LedgerMatch** recibe registros con importe facturado e importe pagado, los valida usando tipos adecuados para negocio y resume qué coincide y qué requiere revisión. Durante 17 lecciones añadirá estructura de paquete, CLI, SQLite, reportes, logging, typing, pruebas y hardening de entradas.

La aplicación vive en [`app/`](app/).

## Tooling verificado

- **CPython 3.14.7**;
- `venv` + `pip`;
- **pytest 9.1.1** para pruebas;
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

Activa el entorno y, para ejecutar pruebas, instala:

```bash
python -m pip install pytest==9.1.1
```

## Run

Linux/macOS:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv
```

PowerShell:

```powershell
$env:PYTHONPATH = 'src'
python -m ledgermatch examples/invoices.csv
```

Filtros útiles:

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --only-differences
PYTHONPATH=src python -m ledgermatch examples/invoices.csv --customer "Cliente Uno" --by-customer
```

## Test

```bash
python -m pytest
```

## Qué sabrás hacer al terminar

La meta es que puedas leer y escribir Python sencillo e idiomático; modelar datos; trabajar con colecciones, archivos y excepciones; separar responsabilidades; usar módulos y paquetes; persistir información; probar comportamiento; diagnosticar fallos; consultar documentación oficial y modificar una base existente sin receta.

## Ruta del curso

Estado actual: **8 de 17 lecciones implementadas**.

1. [Tu primera conciliación en ejecución](lessons/01-tu-primera-conciliacion.md)
2. [Datos de negocio: strings, fechas, Decimal y dataclasses](lessons/02-datos-y-tipos.md)
3. [Validación y errores de entrada](lessons/03-validacion-y-errores.md)
4. [Pruebas con pytest y primer checkpoint](lessons/04-pruebas-y-checkpoint.md)
5. [Colecciones, agrupación y duplicados](lessons/05-colecciones-y-agrupacion.md)
6. [Funciones, comprehensions e iteración](lessons/06-funciones-comprensiones-e-iteracion.md)
7. [Argumentos de CLI, rutas y archivos](lessons/07-cli-rutas-y-archivos.md)
8. [Módulos, responsabilidades y checkpoint 02](lessons/08-modulos-responsabilidades-y-checkpoint.md)
9. Persistencia con SQLite
10. Transacciones e idempotencia
11. Reportes CSV y JSON
12. Configuración, logging y checkpoint 03
13. Type hints, protocolos y contratos internos
14. Entornos, dependencias y empaquetado moderno
15. Debugging, profiling y diagnóstico
16. Seguridad de entradas y hardening + checkpoint 04
17. Evaluación final sin receta

## Checkpoints

- después de la lección 4: [`checkpoint-01`](exercises/checkpoint-01.md), con [`solución de referencia`](solutions/checkpoint-01.md);
- después de la lección 8: [`checkpoint-02`](exercises/checkpoint-02.md), con [`solución de referencia`](solutions/checkpoint-02.md).

## ¿Qué tipo de trabajo utiliza estas habilidades?

Son fundamentos transferibles a automatización, ETL ligero, scripts operativos, integraciones, mantenimiento de aplicaciones Python y backend. El curso busca evidencia práctica; no garantiza contratación ni sustituye experiencia real en equipos.

## Preguntas frecuentes

### ¿Por qué no empezamos con Django, FastAPI, pandas o IA?
Porque antes de depender de frameworks conviene dominar el lenguaje, archivos, tipos, errores, módulos y pruebas. LedgerMatch podrá crecer sin esconder esos fundamentos.

### ¿Por qué `Decimal` y no `float` para dinero?
Porque una conciliación financiera necesita representar decimales de forma explícita y predecible. La lección 2 explica el motivo con código.

### ¿Por qué un CLI?
Porque permite resolver un problema real con muy poca infraestructura y hace visibles los fundamentos propios de Python.

### ¿Tengo que aprender Git aquí?
No. Git tendrá su propio curso.

## Glosario inicial

- **Intérprete:** programa que ejecuta código Python.
- **Módulo:** archivo Python que puede importarse.
- **Paquete:** conjunto organizado de módulos.
- **CSV:** formato tabular de texto separado por delimitadores.
- **Decimal:** tipo para aritmética decimal exacta en casos como dinero.
- **dataclass:** forma concisa de declarar clases orientadas principalmente a datos.
- **pytest:** herramienta de pruebas utilizada por el curso.
- **comprehension:** sintaxis compacta para construir o seleccionar elementos a partir de un iterable.
- **argparse:** módulo estándar para construir interfaces de línea de comandos.

## Cómo hablar de este proyecto en una entrevista

Explica el problema antes que las bibliotecas: recibiste datos tabulares de facturación/pagos, modelaste dinero sin `float`, separaste parsing de conciliación, convertiste entradas defectuosas en errores controlados, agrupaste y filtraste resultados con colecciones, construiste una CLI con argumentos y separaste formato de I/O para mantener pruebas pequeñas.

## Referencias oficiales

- [Documentación de Python 3.14](https://docs.python.org/3.14/)
- [`csv`](https://docs.python.org/3.14/library/csv.html)
- [`decimal`](https://docs.python.org/3.14/library/decimal.html)
- [`dataclasses`](https://docs.python.org/3.14/library/dataclasses.html)
- [`argparse`](https://docs.python.org/3.14/library/argparse.html)
- [`pathlib`](https://docs.python.org/3.14/library/pathlib.html)
- [`sqlite3`](https://docs.python.org/3.14/library/sqlite3.html)
- [Python Packaging User Guide](https://packaging.python.org/)
- [pytest](https://docs.pytest.org/)

## Siguiente paso

Si empiezas desde cero, ve a la [Lección 1](lessons/01-tu-primera-conciliacion.md). Si ya completaste el checkpoint 01, continúa con la [Lección 5](lessons/05-colecciones-y-agrupacion.md).

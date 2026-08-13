# Lección 12 — Configuración, logging y tercer checkpoint

## Qué vas a conseguir

Vas a separar configuración de comportamiento, dejar trazas operativas sin imprimir datos sensibles y extender la persistencia sin instrucciones línea por línea.

## El problema

Una ruta SQLite escrita directamente dentro del código funciona sólo hasta que:

- ejecutas pruebas aisladas;
- quieres otra base por entorno;
- un operador necesita cambiarla sin editar Python.

Y cuando una importación falla o se repite, `print()` indiscriminado no es un sistema de diagnóstico.

## Concepto: configuración explícita

`config.py` carga una ruta de base de datos con este orden:

1. `--db` si el usuario la proporcionó;
2. `LEDGERMATCH_DB` si existe;
3. `ledgermatch.db` como default local.

La prioridad es visible y probabile.

```python
settings = load_settings(database=args.db)
```

La configuración no contiene secretos en este proyecto. Si un sistema real tuviera credenciales, tampoco deberían terminar hardcodeadas en el repositorio.

## Concepto: logging

`logging` pertenece a la biblioteca estándar. En `__main__.py` creamos:

```python
logger = logging.getLogger(__name__)
```

y configuramos el nivel desde `--verbose`.

El evento de importación registra:

- nombre del archivo;
- `run_id`;
- si fue creada o reutilizada.

No registra todo el CSV ni filas completas.

## Demostración

[EJECUTAR]

```bash
PYTHONPATH=src python -m ledgermatch examples/invoices.csv \
  --db demo.db \
  --verbose
```

Ejecuta de nuevo y observa `created=False` en el diagnóstico.

## Código real

Logging usa placeholders:

```python
logger.info(
    "Importación %s (run_id=%s, created=%s)",
    args.csv_file.name,
    outcome.save.run_id,
    outcome.save.created,
)
```

No necesita construir el mensaje si el nivel no está habilitado.

## Qué acaba de pasar

La aplicación distingue tres tipos de salida:

- **resultado de negocio**: reporte;
- **diagnóstico operativo**: logging;
- **código de salida**: señal para scripts y CI.

No son la misma responsabilidad.

## Errores comunes

- imprimir configuraciones completas que podrían contener secretos;
- usar `ERROR` para eventos normales;
- guardar una ruta de desarrollo absoluta en el código;
- usar logs como sustituto de una prueba;
- capturar errores y devolver siempre código 0.

## Buenas prácticas

- define precedencia de configuración;
- valida valores en la frontera;
- usa niveles de logging con intención;
- registra identificadores y decisiones, no payloads completos;
- conserva códigos de salida útiles para automatización.

## Tu turno — Checkpoint 03

Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

La historia pide agregar a `list_runs()` un filtro opcional por nombre de archivo fuente, sin duplicar SQL ni romper el orden actual.

Debes:

1. extender el contrato público;
2. parametrizar el filtro;
3. mantener el comportamiento sin filtro;
4. agregar pruebas para ambos caminos;
5. decidir si la comparación será exacta o normalizada y documentarlo.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

```bash
python -m pytest
```

## Solución

Después de un intento completo, compara con [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).

## Reto adicional

¿Por qué no debería `config.py` importar `SqliteRunRepository`? Explica qué responsabilidad perdería claridad.

## Resumen

- configuración no es lógica de negocio;
- una precedencia explícita evita sorpresas;
- `logging` es diagnóstico estructurado;
- los logs no deben convertirse en una copia de datos;
- el checkpoint exige extender persistencia conservando contratos.

## Siguiente paso

En la [Lección 13](13-type-hints-protocolos-y-contratos.md) haremos explícito, mediante typing estructural, qué necesita el servicio de importación de cualquier repositorio compatible.

## Referencias

- [`logging`](https://docs.python.org/3.14/library/logging.html)
- [Logging HOWTO](https://docs.python.org/3.14/howto/logging.html)
- [`os.environ`](https://docs.python.org/3.14/library/os.html#os.environ)

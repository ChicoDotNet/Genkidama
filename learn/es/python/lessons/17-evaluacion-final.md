# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir

Vas a demostrar lectura de código existente, funcionalidad nueva, bugfix, manejo de errores, pruebas, consulta de documentación y defensa de decisiones. Esta lección no introduce un concepto principal nuevo.

## Antes de empezar

```bash
python -m pytest
python -m pip install -e ".[dev]"
ledgermatch --help
```

## El encargo

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md). No contiene una lista de líneas que debas editar.

Puedes consultar documentación oficial, lecciones, mensajes de error y help de herramientas. No abras la solución hasta completar un intento.

## Qué se evaluará

Usa la [`rúbrica final`](../exercises/rubrica-final.md): comportamiento, pruebas, tipos, errores, separación de responsabilidades, seguridad básica, claridad y explicación.

## Comprobación mínima

```bash
python -m pytest
python -m pip wheel --no-deps . -w dist
ledgermatch examples/invoices.csv --db final.db
```

## Defensa técnica

Prepárate para explicar el recorrido CLI→servicio→SQLite; por qué `Decimal`; dónde vive idempotencia; qué aporta `RunRepository`; controles de archivos; cómo diagnosticarías un archivo grande y qué faltaría para un sistema multiusuario.

## Cómo hablar de este proyecto en una entrevista

Explica problema, decisiones, confiabilidad, operación y un tradeoff. Ejemplo: SQLite es adecuado para esta herramienta local, pero no lo presentarías automáticamente como arquitectura distribuida.

## Solución de referencia

Sólo después de tu intento, compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). Mide contra la rúbrica, no similitud de líneas.

## Resumen

El ciclo final es: **leer → formular → probar → implementar → diagnosticar → verificar → explicar**.

Esto no garantiza empleo; sí produce evidencia concreta de habilidades iniciales sobre una aplicación que ya no es un ejercicio de sintaxis.

## Siguiente paso

Conserva el proyecto como evidencia, repite la evaluación donde falló la rúbrica y continúa con proyectos propios o el siguiente lenguaje de Genkidama Learn.

## Referencias

- [Python 3.14](https://docs.python.org/3.14/)
- [Python Packaging User Guide](https://packaging.python.org/)
- [pytest](https://docs.pytest.org/)

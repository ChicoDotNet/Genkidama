# Lección 4 — Pruebas con pytest y primer checkpoint

## Qué vas a conseguir

Vas a ejecutar pruebas automáticas sobre parsing y conciliación, leer su estructura y usar una regresión como red de seguridad antes del primer cambio autónomo.

## Antes de empezar

Desde `learn/es/python/app`, instala la dependencia de desarrollo:

```bash
python -m pip install pytest==9.1.1
```

Después ejecuta:

```bash
python -m pytest
```

## El problema

LedgerMatch ya tiene varias decisiones: dinero decimal, columnas obligatorias, filas inválidas y signo de diferencias. Probarlas manualmente cada vez no escala.

Necesitamos evidencia repetible.

## Concepto

Una prueba sigue una idea simple:

**preparar → ejecutar → comprobar**.

`pytest` descubre funciones cuyo nombre empieza con `test_`. La fixture `tmp_path` entrega una carpeta temporal aislada, ideal para probar archivos sin ensuciar el repositorio.

## Demostración

Abre `tests/test_reconciler.py` y localiza:

```python
def test_reconcile_counts_matches_and_differences(tmp_path):
```

La prueba crea un CSV mínimo, llama a las mismas funciones que usa la aplicación y verifica conteos, totales y diferencia.

[EJECUTAR]

```bash
python -m pytest -v
```

## Código real

Observa esta afirmación:

```python
assert summary.lines[1].difference == Decimal("-0.50")
```

No comprueba “que no truene”. Define un comportamiento concreto.

Otra prueba usa:

```python
with pytest.raises(CsvSchemaError):
```

También los fallos esperados forman parte del contrato.

## Qué acaba de pasar

Ahora puedes refactorizar parsing o conciliación y detectar si cambias una regla observable. Las pruebas no prueban que la arquitectura sea perfecta; reducen la probabilidad de romper lo que ya declaraste correcto.

## Errores comunes

- Escribir una prueba que sólo repite la implementación.
- Comprobar detalles internos sin valor de negocio.
- Deshabilitar una prueba incómoda para tener CI verde.
- Usar un único archivo gigantesco con todos los escenarios.
- Pensar que cobertura alta equivale automáticamente a buen diseño.

## Buenas prácticas

Nombra la prueba por comportamiento. Haz que falle por una sola razón comprensible. Mantén fixtures pequeñas y usa datos que permitan calcular el resultado a mano.

## Tu turno

Cambia temporalmente en `reconciler.py` la resta por el orden contrario. Ejecuta las pruebas. Debe fallar la regresión del signo.

Revierte el cambio y confirma verde.

[PAUSA PARA EJERCICIO]

Ahora resuelve [`checkpoint-01`](../exercises/checkpoint-01.md) sin abrir la solución.

## Cómo comprobar

Antes del checkpoint debes tener:

```text
3 passed
```

Tu solución del checkpoint debe agregar pruebas nuevas y mantener las anteriores verdes.

## Solución

La solución del cambio temporal es simplemente revertirlo: `payment_total - invoice_total` conserva el contrato actual.

Para el checkpoint utiliza la [`solución de referencia`](../solutions/checkpoint-01.md) sólo después de hacer un intento completo.

## Reto adicional

Ejecuta una sola prueba por nombre con `pytest -k`. Esto será útil cuando la suite crezca.

## Resumen

- Una prueba automatiza evidencia, no confianza ciega.
- `pytest` mantiene el código de prueba pequeño y legible.
- `tmp_path` permite probar I/O sin archivos permanentes.
- Ya puedes cambiar LedgerMatch con una red de seguridad mínima.

## Siguiente paso

La lección 5 usará colecciones para detectar duplicados y agrupar resultados. Antes de avanzar, completa el checkpoint.

## Referencias

- [pytest: Get Started](https://docs.pytest.org/en/stable/getting-started.html)
- [`pathlib`](https://docs.python.org/3.14/library/pathlib.html)

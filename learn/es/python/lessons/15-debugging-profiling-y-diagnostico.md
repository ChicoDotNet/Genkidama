# Lección 15 — Debugging, profiling y diagnóstico

## Qué vas a conseguir

Vas a elegir entre prueba, debugger, logs y profiler según la pregunta que necesitas responder.

## El problema

“Resultado equivocado” y “está lento” requieren investigaciones diferentes. Editar código antes de medir sólo aumenta incertidumbre.

## Secuencia

1. reproduce;
2. conserva entrada/resultado esperado;
3. ejecuta la prueba cercana;
4. lee error/logs;
5. formula hipótesis;
6. usa debugger para estado;
7. usa profiler para rendimiento;
8. cambia lo mínimo;
9. deja regresión;
10. ejecuta suite completa.

## `pdb`

```bash
python -m pdb -m ledgermatch examples/invoices.csv --db demo.db
```

Permite breakpoints, stepping, stack e inspección interactiva.

## `cProfile`

```bash
python -m cProfile -s cumulative -m ledgermatch examples/invoices.csv --db demo.db
```

Mide antes de optimizar. Con cuatro filas, no encontrar una necesidad de optimización también es una conclusión válida.

## Logging

`--verbose` deja contexto operacional: archivo, run id y si la corrida fue nueva/reutilizada; no copia todo el CSV.

## Errores comunes

- optimizar sin medir;
- loguear cada payload;
- arreglar sólo en debugger sin prueba;
- extrapolar un perfil diminuto a cualquier carga.

## Tu turno

Provoca temporalmente un bug en `difference`, demuestra una prueba roja, inspecciona con `pdb`, revierte y confirma verde. Después perfila el ejemplo y anota las funciones con mayor acumulado sin optimizarlas automáticamente.

## Cómo comprobar

```bash
python -m pytest
python -m cProfile -s cumulative -m ledgermatch examples/invoices.csv --db demo.db
```

## Resumen

- pruebas conservan expectativas;
- `pdb` observa estado;
- `cProfile` mide tiempo;
- logging deja evidencia operacional;
- diagnosticar primero reduce cambios especulativos.

## Siguiente paso

En la [Lección 16](16-seguridad-de-entradas-y-hardening.md) endureceremos fronteras de archivos antes de la evaluación final.

## Referencias

- [`pdb`](https://docs.python.org/3.14/library/pdb.html)
- [Profilers](https://docs.python.org/3.14/library/profile.html)
- [`logging`](https://docs.python.org/3.14/library/logging.html)

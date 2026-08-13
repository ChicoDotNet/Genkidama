# Lección 14 — Tooling y gate profesional

## Qué vas a conseguir

Convertirás la validación de NominaBatch en un comando local único que también puede ejecutar CI, evitando que cada persona recuerde una secuencia diferente.

## Antes de empezar

Completa la [Lección 13](13-organizacion-y-fronteras.md).

## El problema

Compilar manualmente, ejecutar el programa y revisar a ojo `report.txt` funciona una vez. En un equipo, ese proceso se degrada: alguien olvida `-Wall`, otra persona no prueba fallos operativos y CI termina validando algo distinto a lo que se ejecuta localmente.

## Concepto

Un gate útil debe ser **aburrido, reproducible y proporcional**. Este curso no necesita un framework pesado: `cobc`, shell y aserciones concretas bastan para demostrar comportamiento.

`tools/verify.sh` se convierte en la entrada única. Ejecuta el smoke funcional y después los escenarios operativos negativos. El workflow de GitHub Actions invoca el mismo comando.

## Demostración

[EJECUTAR]

```bash
cd app
bash tools/verify.sh
```

Debes ver tres señales finales: smoke funcional OK, fallos operativos OK y verify OK.

## Código real

`tests/smoke.sh` prueba resultados de negocio. `tests/operational.sh` prueba códigos de salida y diagnósticos cuando falta la entrada o no puede crearse el reporte. `tools/verify.sh` sólo orquesta ambos; no duplica sus aserciones.

## Qué acaba de pasar

Local y CI comparten el mismo contrato ejecutable. Si una prueba falla en GitHub, puedes reproducir primero el mismo comando en tu entorno.

## Errores comunes

- escribir un script que siempre termina con cero;
- ocultar errores para conseguir verde;
- tener comandos distintos en README y CI;
- instalar linters pesados sin una señal concreta;
- confiar sólo en que el compilador produjo un binario.

## Buenas prácticas

Mantén `set -euo pipefail`, limpia artefactos temporales y prueba tanto camino feliz como fallos importantes. Un gate verde significa únicamente que sus contratos observados pasaron; no prueba ausencia total de defectos.

## Tu turno

Modifica temporalmente una expectativa del smoke para comprobar que `tools/verify.sh` falla. Revierte el cambio y confirma verde otra vez.

## Cómo comprobar

```bash
bash tools/verify.sh
```

Debe devolver código `0` únicamente cuando ambos grupos de pruebas pasan.

## Solución enlazada

La solución está en el propio gate: estudia `tools/verify.sh`, `tests/smoke.sh` y `tests/operational.sh` después de hacer la prueba de fallo controlado.

## Reto adicional

Propón una comprobación adicional que dé señal real sin instalar dependencias nuevas. Explica qué riesgo cubre.

## Resumen

Un comando reproducible reduce diferencias entre desarrollo y CI y hace más barato mantener el batch.

## Siguiente paso

Continúa con la [Lección 15 — diagnóstico y rendimiento](15-diagnostico-y-rendimiento.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- [GitHub Actions documentation](https://docs.github.com/actions)

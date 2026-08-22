# Ejercicio final — Mantén y evoluciona ThreadSeek

## Contexto

Recibes ThreadSeek después de 16 lecciones. El producto ya indexa, busca, persiste, mide, paraleliza, permite cancelación y pasa una matriz de compiladores. Tu trabajo es demostrar mantenimiento profesional, no repetir código de las lecciones.

## Historias

1. Agrega filtro de extensión al descubrimiento sin romper modo secuencial/paralelo.
2. Endurece el parser del índice persistido ante registros ambiguos o fuera de rango.
3. Diseña una frontera capaz de solicitar cancelación desde una interacción externa.
4. Mejora progreso sin exponer buffers internos ni introducir un mutex global del resultado.
5. Ejecuta un experimento 1/2/4/8 workers y explica los datos.
6. Propón la frontera mínima para indexación incremental basada en eventos futuros.

## Entrega

Incluye código, tests, comandos ejecutados, resultado de la matriz soportada, notas de medición y una explicación de una decisión que descartaste.

No se evalúa cuántas abstracciones agregaste. Se evalúa si el cambio es correcto, pequeño, testeable, portable y defendible.

Cuando hayas terminado tu intento, revisa la [solución de referencia](../solutions/final-threadseek.md).

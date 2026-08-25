# Checkpoint 03 — Catálogo y facturación durable

Sin seguir una receta paso a paso:

1. crea una carpeta temporal con dos cotizaciones válidas y un `.quote.json` corrupto;
2. busca una de las cotizaciones por cliente y demuestra que la incidencia corrupta permanece visible;
3. aprueba la cotización encontrada y crea una factura con folio propio;
4. persiste y vuelve a cargar esa factura;
5. añade o ajusta una prueba que proteja un failure mode observado.

La evidencia debe incluir tests verdes y ningún silenciamiento de errores.

Después de intentarlo: [solución de referencia](../solutions/checkpoint-03-solution.md).

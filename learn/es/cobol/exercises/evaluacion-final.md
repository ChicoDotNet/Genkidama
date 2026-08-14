# Evaluación final — NominaBatch sin receta

Trabaja sobre la aplicación canónica después de completar las 16 lecciones y los checkpoints. No abras la solución de referencia hasta terminar un intento completo.

## Historia A — Bono fijo opcional

Extiende el formato de entrada con un campo monetario `bono`: ausencia en registros históricos equivale a `0`; si existe debe ser decimal no negativo; se suma al bruto antes de la deducción; un valor inválido rechaza el registro antes de modificar acumuladores. Agrega regresiones para compatibilidad histórica, bono válido y bono inválido.

## Historia B — Bug de integridad: ID con espacios

Reproduce y corrige el caso donde IDs que sólo difieren por espacios exteriores pueden escapar de la detección de duplicados. Define una política determinista de normalización, rechaza la segunda identidad equivalente antes de modificar totales/bandas/tabla de IDs, produce un diagnóstico útil y agrega una prueba de regresión.

## Historia C — Conserva contratos

Demuestra que siguen funcionando compilación con `cobc -x -free -Wall -I copybooks`, `bash tools/verify.sh`, validaciones actuales, duplicados, totales globales/por bandas, entrada ausente → retorno `2`, salida no disponible → retorno `3` y el límite documentado de IDs. No debilites tests para conseguir verde.

## Historia D — Documentación oficial

Consulta al menos dos secciones oficiales de GnuCOBOL relacionadas con decisiones reales de tu cambio. Entrega enlace, qué verificaste y qué decisión tomaste.

## Historia E — Diseño y operación

En 180–300 palabras explica dónde debe vivir la normalización del ID, qué precisión usarías para el bono, cómo evolucionarías el formato de archivo, qué medirías antes de reemplazar la búsqueda lineal y qué frontera cambiarías si la entrada proviniera de cola o base de datos.

## Entrega

Incluye código, pruebas, comandos ejecutados, comprobación manual, notas de documentación y un error que hayas diagnosticado durante el trabajo.

## Comprobación mínima

Desde `app/`:

```bash
bash tools/verify.sh
./nomina
```

Comprueba un registro legacy sin bono, uno con bono válido, un bono inválido sin contaminación de totales y un ID equivalente sólo por espacios rechazado como duplicado.

Evalúate con [`rubrica-final.md`](rubrica-final.md).

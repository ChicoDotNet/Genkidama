# Lección 8 — Importar/exportar JSON y checkpoint 02

## Qué vas a conseguir
Exportarás el tablero a JSON portable e importarás uno de vuelta con validación explícita. Practicarás `JSON.parse/stringify`, versionado, `Blob`, `File.text()` y validación en una frontera no confiable.

## Antes de empezar
Crea tres tarjetas, muévelas a columnas distintas y ejecuta `npm test`.

## El problema
`localStorage` pertenece a un navegador y origen concretos. Un archivo portable resuelve respaldo/traslado, pero no debe asumirse correcto sólo porque termina en `.json`.

## Concepto
La exportación usa un contrato con `version` y `cards`. `importBoard` valida sintaxis, versión, colección, ids únicos, títulos normalizados y columnas conocidas.

## Demostración
[DEMO] Exporta el tablero y observa `version: 1`. Modifica una columna por un valor no soportado e intenta importar: el documento debe rechazarse completo.

## Código real
`storage.js` separa persistencia local, serialización portable y parseo/validación. `loadBoard` conserva compatibilidad con el formato sin `version` de las primeras lecciones.

## Qué acaba de pasar
JSON válido no significa datos válidos. También introdujimos una migración compatible para no perder el trabajo previo.

## Errores comunes
- confiar sólo en `JSON.parse`;
- importar parcialmente;
- romper datos existentes al cambiar formato;
- olvidar liberar una URL temporal.

## Buenas prácticas
Valida completamente antes de reemplazar estado y versiona formatos que esperas evolucionar.

## Tu turno — Checkpoint 02
Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
Ejecuta `npm run check`, `npm test` y `npm run smoke`; después prueba exportación/importación y una versión incorrecta.

## Solución
Compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md) cuando termines.

## Reto adicional
¿Qué límite razonado pondrías al tamaño de un archivo importado y qué riesgo mitigaría?

## Resumen
JSON es formato, no validación; versionar y migrar evita pérdida de datos.

## Siguiente paso
Continúa con [Lección 9 — Asincronía real](09-asincronia-real.md).

## Referencias
- [`JSON.parse`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse)
- [`Blob`](https://developer.mozilla.org/docs/Web/API/Blob)
- [`File.text()`](https://developer.mozilla.org/docs/Web/API/Blob/text)

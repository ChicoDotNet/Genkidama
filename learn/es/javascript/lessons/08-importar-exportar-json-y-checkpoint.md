# Lección 8 — Importar/exportar JSON y checkpoint 02

## Qué vas a conseguir
Exportarás el tablero a un archivo JSON portable e importarás uno de vuelta con validación explícita. Practicarás `JSON.parse/stringify`, versionado de formato, `Blob`, `File.text()` y validación en una frontera no confiable.

## Antes de empezar
Crea tres tarjetas, muévelas a columnas distintas y ejecuta:

```bash
npm test
```

## El problema
`localStorage` pertenece a un navegador y origen concretos. Si quieres respaldar o trasladar el tablero, necesitas una representación portable. Pero un archivo elegido por el usuario no debe asumirse correcto sólo porque termina en `.json`.

## Concepto
La exportación tiene un contrato explícito:

```json
{
  "version": 1,
  "cards": [
    { "id": "...", "title": "Preparar demo", "column": "todo" }
  ]
}
```

`version` permite distinguir formatos futuros. `importBoard` valida sintaxis JSON, versión, colección de tarjetas, ids únicos, títulos normalizados y columnas conocidas.

## Demostración
[DEMO] Pulsa `Exportar JSON`, abre el archivo en el editor y observa `version: 1`.

Luego importa el mismo archivo. Kanban Local reemplaza el tablero, lo persiste y anuncia cuántas tarjetas recibió.

[EJECUTAR] Cambia manualmente una columna por `archive` e intenta importar. La aplicación debe rechazar el archivo, no guardar media importación.

## Código real
`storage.js` contiene tres responsabilidades de frontera:

- `saveBoard`: persistencia local;
- `exportBoard`: serialización portable y legible;
- `importBoard`: parseo + validación del documento externo.

`loadBoard` conserva compatibilidad con el formato sin `version` usado en las primeras lecciones. Esto evita que el incremento destruya silenciosamente datos que el alumno ya tenía guardados.

En la UI, `Blob` crea el contenido descargable y `URL.createObjectURL` produce una URL temporal. Para importar, el `input type="file"` entrega un `File` y `await file.text()` obtiene el texto.

## Qué acaba de pasar
Apareció una frontera de confianza nueva. JSON válido no significa datos válidos. Por eso `JSON.parse` es sólo el primer paso: después aplica el contrato de dominio.

También hicimos una migración compatible: el formato interno antiguo todavía carga; la siguiente escritura lo deja en formato versionado.

## Errores comunes
- aceptar cualquier objeto que `JSON.parse` pueda leer;
- confiar en la extensión `.json`;
- importar parcialmente antes de terminar validación;
- romper datos existentes al introducir una nueva versión de almacenamiento;
- olvidar `URL.revokeObjectURL` después de una descarga temporal.

## Buenas prácticas
Valida completamente antes de reemplazar estado. Mantén el formato explícitamente versionado si esperas evolucionarlo. Cuando cambies persistencia, piensa en compatibilidad/migración antes de borrar datos.

## Tu turno — Checkpoint 02
Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución. Integrarás edición, eliminación, filtrado, accesibilidad y persistencia en una capacidad pequeña.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

```bash
npm run check
npm test
npm run smoke
```

Después exporta un tablero, recarga la página e impórtalo. También prueba un JSON con versión incorrecta.

## Solución
Cuando termines, compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

## Reto adicional
¿Qué límites pondrías al tamaño de un archivo importado? No implementes todavía una política arbitraria: anota amenaza, costo y criterio. Volveremos a hardening más adelante.

## Resumen
- JSON es formato, no validación;
- una versión explícita permite evolucionar contratos;
- archivos externos son una frontera no confiable;
- compatibilidad hacia atrás evita deuda y pérdida de datos;
- el checkpoint integra el segundo bloque del curso.

## Siguiente paso
La Lección 9 introducirá asincronía con un problema real, no con un `setTimeout` de juguete.

## Referencias
- [`JSON.parse`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse)
- [`JSON.stringify`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)
- [`Blob`](https://developer.mozilla.org/docs/Web/API/Blob)
- [`File.text()`](https://developer.mozilla.org/docs/Web/API/Blob/text)

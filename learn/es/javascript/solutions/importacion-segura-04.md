# Solución de referencia — Checkpoint 04

Esta es una solución posible, no la única correcta. Compara responsabilidades y evidencia, no nombres de archivos exactos.

## 1. Mantén la comparación fuera del DOM
Una opción es crear `src/import-preview.js`:

```js
/**
 * Compare identity-level impact of replacing one board with another.
 * @param {{cards:Array<{id:string}>}} current
 * @param {{cards:Array<{id:string}>}} candidate
 */
export function summarizeReplacement(current, candidate) {
  const currentIds = new Set(current.cards.map((card) => card.id));
  const candidateIds = new Set(candidate.cards.map((card) => card.id));

  return Object.freeze({
    currentCount: current.cards.length,
    candidateCount: candidate.cards.length,
    addedCount: candidate.cards.filter((card) => !currentIds.has(card.id)).length,
    missingCount: current.cards.filter((card) => !candidateIds.has(card.id)).length,
  });
}
```

No necesita saber cómo se obtuvo el JSON ni cómo se mostrará el mensaje.

## 2. Prueba la función pura
Ejemplo:

```js
import test from "node:test";
import assert from "node:assert/strict";
import { summarizeReplacement } from "../src/import-preview.js";

test("resume ids nuevos y ausentes sin mutar tableros", () => {
  const current = { cards: [{ id: "a" }, { id: "b" }] };
  const candidate = { cards: [{ id: "b" }, { id: "c" }] };

  const result = summarizeReplacement(current, candidate);

  assert.deepEqual(result, {
    currentCount: 2,
    candidateCount: 2,
    addedCount: 1,
    missingCount: 1,
  });
  assert.deepEqual(current.cards.map((card) => card.id), ["a", "b"]);
  assert.deepEqual(candidate.cards.map((card) => card.id), ["b", "c"]);
});
```

Añade los casos mínimos pedidos por el ejercicio.

## 3. Decide en la capa de UI
En `app.js`, el candidato se mantiene en una variable local hasta confirmar:

```js
assertImportFileSize(file.size);
const text = await diagnostics.measureAsync("import.read", () => file.text());
const candidate = diagnostics.measure("import.parse", () => importBoard(text));
const preview = summarizeReplacement(board, candidate);

const accepted = window.confirm(
  `Tablero actual: ${preview.currentCount}\n` +
  `Archivo: ${preview.candidateCount}\n` +
  `Nuevas: ${preview.addedCount}\n` +
  `Ausentes: ${preview.missingCount}\n\n` +
  "¿Reemplazar el tablero?",
);

if (!accepted) {
  setStatus("Importación cancelada; el tablero no cambió.");
  return;
}

board = candidate;
searchInput.value = "";
columnFilter.value = "all";
await persistAndRender(`Tablero importado: ${board.cards.length} tarjeta(s).`);
```

La propiedad importante es el orden: **parsear y resumir no modifica el estado persistido; asignar y guardar ocurre sólo después de confirmar**.

## 4. Mantén la PWA coherente
Si `app.js` importa `./import-preview.js`, agrega el módulo al `APP_SHELL` y cambia el nombre/version del cache. El gate `npm run check` debería fallar si olvidas la app shell.

Eso convierte una obligación de documentación en una defensa ejecutable.

## 5. No confundas validación con confirmación
El candidato debe estar completamente validado **antes** de pedir permiso. La confirmación responde “¿quiero este cambio?”, no “¿es confiable el formato?”.

Igualmente, cancelar no necesita “deshacer”: todavía no debe haber nada que deshacer.

## 6. Qué hablar en una revisión de código
Una explicación profesional sería:

> Separé el cálculo del impacto en una función pura para poder probarlo sin navegador. La UI conserva el tablero candidato como dato temporal y sólo reemplaza/persiste después de confirmación. La validación existente sigue siendo la única fuente de invariantes del formato.

Eso demuestra responsabilidad, pruebas y orden de efectos sin sobrearquitectura.

## Comprobación final
Exige:

```powershell
npm run verify
npm start
```

Después prueba manualmente aceptar, cancelar, JSON inválido y archivo que exceda el límite.

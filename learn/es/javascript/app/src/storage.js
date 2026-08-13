import { assertValidBoard } from "./board.js";

const STORAGE_KEY = "genkidama-kanban-v1";
const EXPORT_VERSION = 1;
/** Defensive limit before reading an imported file into memory. */
export const MAX_IMPORT_BYTES = 1_000_000;

/** Validate a File-like byte size before reading its contents. @param {number} size @returns {number} */
export function assertImportFileSize(size) {
  if (!Number.isSafeInteger(size) || size < 0) {
    throw new TypeError("El tamaño del archivo debe ser un entero no negativo.");
  }
  if (size > MAX_IMPORT_BYTES) {
    throw new RangeError(`El archivo supera el límite de ${MAX_IMPORT_BYTES} bytes.`);
  }
  return size;
}

/** Load a board through a Storage-compatible reader, including the pre-versioned lesson format. @param {{getItem(key:string): string|null}} storage @returns {{cards:Array<object>}} */
export function loadBoard(storage) {
  const raw = storage.getItem(STORAGE_KEY);
  if (!raw) return { cards: [] };
  try {
    const parsed = JSON.parse(raw);
    if (parsed && typeof parsed === "object" && Array.isArray(parsed.cards)) {
      return assertValidBoard({ cards: parsed.cards });
    }
    return { cards: [] };
  } catch {
    return { cards: [] };
  }
}

/** Persist a board through a Storage-compatible writer. @param {{setItem(key:string,value:string):void}} storage @param {{cards:Array<object>}} board */
export function saveBoard(storage, board) {
  assertValidBoard(board);
  storage.setItem(STORAGE_KEY, JSON.stringify({ version: EXPORT_VERSION, cards: board.cards }));
}

/** Serialize a validated board as portable, versioned JSON. @param {{cards:Array<object>}} board @returns {string} */
export function exportBoard(board) {
  assertValidBoard(board);
  return JSON.stringify({ version: EXPORT_VERSION, cards: board.cards }, null, 2);
}

/** Parse and validate one portable Kanban Local JSON document. @param {string} text @returns {{cards:Array<object>}} */
export function importBoard(text) {
  if (typeof text !== "string") {
    throw new TypeError("El contenido importado debe ser texto JSON.");
  }

  let parsed;
  try {
    parsed = JSON.parse(text);
  } catch {
    throw new SyntaxError("El archivo no contiene JSON válido.");
  }

  if (!parsed || typeof parsed !== "object" || parsed.version !== EXPORT_VERSION) {
    throw new Error(`La exportación debe usar version ${EXPORT_VERSION}.`);
  }

  return assertValidBoard({ cards: parsed.cards });
}

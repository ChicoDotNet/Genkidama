import { loadBoard, saveBoard } from "./storage.js";

/** Load IndexedDB state when possible and use the compatible localStorage board otherwise. */
export async function loadPreferredBoard({ indexedDbFactory, storage, loadIndexedDb }) {
  try {
    const indexedBoard = await loadIndexedDb(indexedDbFactory);
    if (indexedBoard) return { board: indexedBoard, mode: "indexeddb" };
  } catch {
    // IndexedDB can be unavailable in private or restricted browser contexts.
  }
  return { board: loadBoard(storage), mode: "localstorage" };
}

/** Persist to IndexedDB and keep localStorage as a compatibility fallback. */
export async function savePreferredBoard({ indexedDbFactory, storage, board, saveIndexedDb }) {
  try {
    await saveIndexedDb(indexedDbFactory, board);
    saveBoard(storage, board);
    return "indexeddb";
  } catch {
    saveBoard(storage, board);
    return "localstorage";
  }
}

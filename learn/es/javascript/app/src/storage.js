const STORAGE_KEY = "genkidama-kanban-v1";

/** Load a board through a Storage-compatible reader. @param {{getItem(key:string): string|null}} storage @returns {{cards:Array<object>}} */
export function loadBoard(storage) {
  const raw = storage.getItem(STORAGE_KEY);
  if (!raw) return { cards: [] };
  try {
    const parsed = JSON.parse(raw);
    if (!parsed || !Array.isArray(parsed.cards)) return { cards: [] };
    return { cards: parsed.cards };
  } catch {
    return { cards: [] };
  }
}

/** Persist a board through a Storage-compatible writer. @param {{setItem(key:string,value:string):void}} storage @param {{cards:Array<object>}} board */
export function saveBoard(storage, board) {
  storage.setItem(STORAGE_KEY, JSON.stringify(board));
}

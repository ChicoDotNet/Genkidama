const STORAGE_KEY = "genkidama-kanban-v1";

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

export function saveBoard(storage, board) {
  storage.setItem(STORAGE_KEY, JSON.stringify(board));
}

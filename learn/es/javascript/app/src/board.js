export const COLUMN_IDS = Object.freeze(["todo", "doing", "done"]);

export function createBoard() {
  return { cards: [] };
}

export function normalizeTitle(title) {
  if (typeof title !== "string") {
    throw new TypeError("El título debe ser texto.");
  }
  const normalized = title.trim().replace(/\s+/g, " ");
  if (normalized.length < 3 || normalized.length > 80) {
    throw new RangeError("El título debe tener entre 3 y 80 caracteres.");
  }
  return normalized;
}

export function addCard(board, title, id = crypto.randomUUID()) {
  const normalized = normalizeTitle(title);
  if (board.cards.some((card) => card.id === id)) {
    throw new Error("El identificador de la tarjeta ya existe.");
  }
  return {
    ...board,
    cards: [...board.cards, { id, title: normalized, column: "todo" }],
  };
}

export function moveCard(board, cardId, targetColumn) {
  if (!COLUMN_IDS.includes(targetColumn)) {
    throw new RangeError("La columna destino no existe.");
  }
  let found = false;
  const cards = board.cards.map((card) => {
    if (card.id !== cardId) return card;
    found = true;
    return { ...card, column: targetColumn };
  });
  if (!found) {
    throw new Error("La tarjeta no existe.");
  }
  return { ...board, cards };
}

export function cardsInColumn(board, column) {
  return board.cards.filter((card) => card.column === column);
}

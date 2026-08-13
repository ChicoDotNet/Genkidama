/** @typedef {{ id: string, title: string, column: string }} Card */
/** @typedef {{ cards: Card[] }} Board */

/** Stable column identifiers used by domain and UI code. */
export const COLUMN_IDS = Object.freeze(["todo", "doing", "done"]);

/** Create a new empty board. @returns {Board} */
export function createBoard() {
  return { cards: [] };
}

/** Normalize and validate one user-facing card title. @param {string} title @returns {string} */
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

/** Add one card to the todo column without mutating the input board. @param {Board} board @param {string} title @param {string} [id] @returns {Board} */
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

/** Move one existing card to a known column without mutating the input board. @param {Board} board @param {string} cardId @param {string} targetColumn @returns {Board} */
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

/** Return cards belonging to one column. @param {Board} board @param {string} column @returns {Card[]} */
export function cardsInColumn(board, column) {
  return board.cards.filter((card) => card.column === column);
}

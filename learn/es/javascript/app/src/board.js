/** @typedef {{ id: string, title: string, column: string }} Card */
/** @typedef {{ cards: Card[] }} Board */

/** Stable column identifiers used by domain and UI code. */
export const COLUMN_IDS = Object.freeze(["todo", "doing", "done"]);
/** Defensive upper bound for one local board. */
export const MAX_CARDS = 500;

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

/** Validate a board received from an external boundary and return it unchanged. @param {unknown} value @returns {Board} */
export function assertValidBoard(value) {
  if (!value || typeof value !== "object" || !Array.isArray(value.cards)) {
    throw new TypeError("El tablero debe contener un arreglo de tarjetas.");
  }
  if (value.cards.length > MAX_CARDS) {
    throw new RangeError(`El tablero no puede contener más de ${MAX_CARDS} tarjetas.`);
  }

  const ids = new Set();
  for (const card of value.cards) {
    if (!card || typeof card !== "object") {
      throw new TypeError("Cada tarjeta debe ser un objeto.");
    }
    if (typeof card.id !== "string" || card.id.trim() === "") {
      throw new TypeError("Cada tarjeta necesita un identificador de texto.");
    }
    if (ids.has(card.id)) {
      throw new Error("El tablero contiene identificadores de tarjeta duplicados.");
    }
    ids.add(card.id);

    const normalizedTitle = normalizeTitle(card.title);
    if (normalizedTitle !== card.title) {
      throw new Error("Los títulos importados deben venir normalizados.");
    }
    if (!COLUMN_IDS.includes(card.column)) {
      throw new RangeError("El tablero contiene una columna desconocida.");
    }
  }

  return /** @type {Board} */ (value);
}

/** Add one card to the todo column without mutating the input board. @param {Board} board @param {string} title @param {string} [id] @returns {Board} */
export function addCard(board, title, id = crypto.randomUUID()) {
  if (board.cards.length >= MAX_CARDS) {
    throw new RangeError(`El tablero alcanzó el límite de ${MAX_CARDS} tarjetas.`);
  }
  const normalized = normalizeTitle(title);
  if (board.cards.some((card) => card.id === id)) {
    throw new Error("El identificador de la tarjeta ya existe.");
  }
  return {
    ...board,
    cards: [...board.cards, { id, title: normalized, column: "todo" }],
  };
}

/** Edit one existing card without mutating the input board. @param {Board} board @param {string} cardId @param {string} title @returns {Board} */
export function editCard(board, cardId, title) {
  const normalized = normalizeTitle(title);
  let found = false;
  const cards = board.cards.map((card) => {
    if (card.id !== cardId) return card;
    found = true;
    return { ...card, title: normalized };
  });
  if (!found) {
    throw new Error("La tarjeta no existe.");
  }
  return { ...board, cards };
}

/** Delete one existing card without mutating the input board. @param {Board} board @param {string} cardId @returns {Board} */
export function deleteCard(board, cardId) {
  if (!board.cards.some((card) => card.id === cardId)) {
    throw new Error("La tarjeta no existe.");
  }
  return { ...board, cards: board.cards.filter((card) => card.id !== cardId) };
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

/** Filter cards by optional column and case-insensitive text query. @param {Board} board @param {{query?: string, column?: string}} [options] @returns {Card[]} */
export function filterCards(board, { query = "", column = "all" } = {}) {
  if (typeof query !== "string") {
    throw new TypeError("La búsqueda debe ser texto.");
  }
  if (column !== "all" && !COLUMN_IDS.includes(column)) {
    throw new RangeError("La columna de filtro no existe.");
  }

  const normalizedQuery = query.trim().toLocaleLowerCase("es");
  return board.cards.filter((card) => {
    const matchesColumn = column === "all" || card.column === column;
    const matchesText = normalizedQuery === "" || card.title.toLocaleLowerCase("es").includes(normalizedQuery);
    return matchesColumn && matchesText;
  });
}

import { assertValidBoard } from "./board.js";

const DATABASE_NAME = "genkidama-kanban";
const DATABASE_VERSION = 1;
const STORE_NAME = "boards";
const BOARD_KEY = "current";

function requestAsPromise(request) {
  return new Promise((resolve, reject) => {
    request.addEventListener("success", () => resolve(request.result), { once: true });
    request.addEventListener("error", () => reject(request.error ?? new Error("IndexedDB request failed.")), { once: true });
  });
}

/** Open the Kanban Local IndexedDB database. @param {IDBFactory} indexedDbFactory @returns {Promise<IDBDatabase>} */
export function openBoardDatabase(indexedDbFactory) {
  if (!indexedDbFactory || typeof indexedDbFactory.open !== "function") {
    return Promise.reject(new TypeError("IndexedDB no está disponible."));
  }

  return new Promise((resolve, reject) => {
    const request = indexedDbFactory.open(DATABASE_NAME, DATABASE_VERSION);
    request.addEventListener("upgradeneeded", () => {
      const database = request.result;
      if (!database.objectStoreNames.contains(STORE_NAME)) database.createObjectStore(STORE_NAME);
    }, { once: true });
    request.addEventListener("success", () => resolve(request.result), { once: true });
    request.addEventListener("error", () => reject(request.error ?? new Error("No se pudo abrir IndexedDB.")), { once: true });
  });
}

/** Load the current board from IndexedDB. @param {IDBFactory} indexedDbFactory @returns {Promise<{cards:Array<object>}|null>} */
export async function loadBoardFromIndexedDb(indexedDbFactory) {
  const database = await openBoardDatabase(indexedDbFactory);
  try {
    const transaction = database.transaction(STORE_NAME, "readonly");
    const stored = await requestAsPromise(transaction.objectStore(STORE_NAME).get(BOARD_KEY));
    if (stored === undefined) return null;
    return assertValidBoard(stored);
  } finally {
    database.close();
  }
}

/** Save the current board in IndexedDB. @param {IDBFactory} indexedDbFactory @param {{cards:Array<object>}} board @returns {Promise<void>} */
export async function saveBoardToIndexedDb(indexedDbFactory, board) {
  assertValidBoard(board);
  const database = await openBoardDatabase(indexedDbFactory);
  try {
    const transaction = database.transaction(STORE_NAME, "readwrite");
    await requestAsPromise(transaction.objectStore(STORE_NAME).put(board, BOARD_KEY));
  } finally {
    database.close();
  }
}

import { addCard, COLUMN_IDS, deleteCard, editCard, filterCards, moveCard } from "./board.js";
import { loadBoardFromIndexedDb, saveBoardToIndexedDb } from "./idb-storage.js";
import { loadPreferredBoard, savePreferredBoard } from "./repository.js";
import { exportBoard, importBoard } from "./storage.js";

const boardElement = document.querySelector("#board");
const form = document.querySelector("#new-card-form");
const titleInput = document.querySelector("#card-title");
const errorElement = document.querySelector("#form-error");
const searchInput = document.querySelector("#search-cards");
const columnFilter = document.querySelector("#column-filter");
const exportButton = document.querySelector("#export-board");
const importInput = document.querySelector("#import-board");
const statusElement = document.querySelector("#status-message");
let board = { cards: [] };
let persistenceMode = "localstorage";

function setStatus(message) { statusElement.textContent = message; }

async function persistAndRender(message) {
  persistenceMode = await savePreferredBoard({ indexedDbFactory: window.indexedDB, storage: window.localStorage, board, saveIndexedDb: saveBoardToIndexedDb });
  render();
  setStatus(`${message} Persistencia: ${persistenceMode}.`);
}

function render() {
  const visibleCards = filterCards(board, { query: searchInput.value, column: columnFilter.value });
  for (const column of COLUMN_IDS) {
    const list = boardElement.querySelector(`[data-column="${column}"] .cards`);
    list.replaceChildren();
    for (const card of visibleCards.filter((candidate) => candidate.column === column)) {
      const item = document.createElement("li"); item.className = "card"; item.dataset.cardId = card.id;
      const text = document.createElement("span"); text.className = "card-title"; text.textContent = card.title; item.append(text);
      const actions = document.createElement("div"); actions.className = "card-actions";
      for (const [action, label] of [["edit", "Editar"], ["delete", "Eliminar"]]) {
        const button = document.createElement("button"); button.type = "button"; button.dataset.action = action; button.textContent = label;
        button.setAttribute("aria-label", `${label} ${card.title}`); actions.append(button);
      }
      for (const target of COLUMN_IDS.filter((candidate) => candidate !== column)) {
        const button = document.createElement("button"); button.type = "button"; button.dataset.action = "move"; button.dataset.moveTo = target;
        button.textContent = `Mover a ${target}`; button.setAttribute("aria-label", `Mover ${card.title} a ${target}`); actions.append(button);
      }
      item.append(actions); list.append(item);
    }
  }
}

form.addEventListener("submit", async (event) => {
  event.preventDefault(); errorElement.textContent = "";
  try { board = addCard(board, titleInput.value); form.reset(); titleInput.focus(); await persistAndRender("Tarjeta agregada."); }
  catch (error) { errorElement.textContent = error.message; }
});

boardElement.addEventListener("click", async (event) => {
  const button = event.target.closest("button[data-action]"); if (!button) return;
  const cardElement = button.closest("[data-card-id]"); if (!cardElement) return;
  const cardId = cardElement.dataset.cardId; const currentCard = board.cards.find((card) => card.id === cardId); if (!currentCard) return;
  errorElement.textContent = "";
  try {
    if (button.dataset.action === "move") { board = moveCard(board, cardId, button.dataset.moveTo); await persistAndRender("Tarjeta movida."); return; }
    if (button.dataset.action === "edit") { const title = window.prompt("Nuevo título", currentCard.title); if (title === null) return; board = editCard(board, cardId, title); await persistAndRender("Tarjeta editada."); return; }
    if (button.dataset.action === "delete") { if (!window.confirm(`¿Eliminar "${currentCard.title}"?`)) return; board = deleteCard(board, cardId); await persistAndRender("Tarjeta eliminada."); }
  } catch (error) { errorElement.textContent = error.message; }
});

searchInput.addEventListener("input", render);
columnFilter.addEventListener("change", render);
exportButton.addEventListener("click", () => { const blob = new Blob([exportBoard(board)], { type: "application/json" }); const url = URL.createObjectURL(blob); const link = document.createElement("a"); link.href = url; link.download = "kanban-local.json"; link.click(); URL.revokeObjectURL(url); setStatus("Tablero exportado como JSON."); });
importInput.addEventListener("change", async () => { const [file] = importInput.files; if (!file) return; errorElement.textContent = ""; try { board = importBoard(await file.text()); searchInput.value = ""; columnFilter.value = "all"; await persistAndRender(`Tablero importado: ${board.cards.length} tarjeta(s).`); } catch (error) { errorElement.textContent = error.message; } finally { importInput.value = ""; } });

async function initialize() {
  const loaded = await loadPreferredBoard({ indexedDbFactory: window.indexedDB, storage: window.localStorage, loadIndexedDb: loadBoardFromIndexedDb });
  board = loaded.board; persistenceMode = loaded.mode;
  if (loaded.mode === "localstorage" && board.cards.length > 0) persistenceMode = await savePreferredBoard({ indexedDbFactory: window.indexedDB, storage: window.localStorage, board, saveIndexedDb: saveBoardToIndexedDb });
  render(); setStatus(`Tablero listo. Persistencia: ${persistenceMode}.`);
  if ("serviceWorker" in navigator) { try { await navigator.serviceWorker.register("./service-worker.js"); } catch { setStatus(`Tablero listo. Persistencia: ${persistenceMode}. El modo offline no pudo registrarse.`); } }
}

initialize().catch((error) => { errorElement.textContent = `No se pudo iniciar el tablero: ${error.message}`; });

import {
  addCard,
  COLUMN_IDS,
  deleteCard,
  editCard,
  filterCards,
  moveCard,
} from "./board.js";
import { exportBoard, importBoard, loadBoard, saveBoard } from "./storage.js";

const boardElement = document.querySelector("#board");
const form = document.querySelector("#new-card-form");
const titleInput = document.querySelector("#card-title");
const errorElement = document.querySelector("#form-error");
const searchInput = document.querySelector("#search-cards");
const columnFilter = document.querySelector("#column-filter");
const exportButton = document.querySelector("#export-board");
const importInput = document.querySelector("#import-board");
const statusElement = document.querySelector("#status-message");
let board = loadBoard(window.localStorage);

function setStatus(message) {
  statusElement.textContent = message;
}

function persistAndRender(message) {
  saveBoard(window.localStorage, board);
  render();
  setStatus(message);
}

function render() {
  const visibleCards = filterCards(board, {
    query: searchInput.value,
    column: columnFilter.value,
  });

  for (const column of COLUMN_IDS) {
    const list = boardElement.querySelector(`[data-column="${column}"] .cards`);
    list.replaceChildren();

    for (const card of visibleCards.filter((candidate) => candidate.column === column)) {
      const item = document.createElement("li");
      item.className = "card";
      item.dataset.cardId = card.id;

      const text = document.createElement("span");
      text.className = "card-title";
      text.textContent = card.title;
      item.append(text);

      const actions = document.createElement("div");
      actions.className = "card-actions";

      const editButton = document.createElement("button");
      editButton.type = "button";
      editButton.dataset.action = "edit";
      editButton.textContent = "Editar";
      editButton.setAttribute("aria-label", `Editar ${card.title}`);
      actions.append(editButton);

      const deleteButton = document.createElement("button");
      deleteButton.type = "button";
      deleteButton.dataset.action = "delete";
      deleteButton.textContent = "Eliminar";
      deleteButton.setAttribute("aria-label", `Eliminar ${card.title}`);
      actions.append(deleteButton);

      for (const target of COLUMN_IDS.filter((candidate) => candidate !== column)) {
        const button = document.createElement("button");
        button.type = "button";
        button.dataset.action = "move";
        button.dataset.moveTo = target;
        button.textContent = `Mover a ${target}`;
        button.setAttribute("aria-label", `Mover ${card.title} a ${target}`);
        actions.append(button);
      }

      item.append(actions);
      list.append(item);
    }
  }
}

form.addEventListener("submit", (event) => {
  event.preventDefault();
  errorElement.textContent = "";
  try {
    board = addCard(board, titleInput.value);
    form.reset();
    titleInput.focus();
    persistAndRender("Tarjeta agregada.");
  } catch (error) {
    errorElement.textContent = error.message;
  }
});

boardElement.addEventListener("click", (event) => {
  const button = event.target.closest("button[data-action]");
  if (!button) return;
  const cardElement = button.closest("[data-card-id]");
  if (!cardElement) return;
  const cardId = cardElement.dataset.cardId;
  const currentCard = board.cards.find((card) => card.id === cardId);
  if (!currentCard) return;

  errorElement.textContent = "";
  try {
    if (button.dataset.action === "move") {
      board = moveCard(board, cardId, button.dataset.moveTo);
      persistAndRender("Tarjeta movida.");
      return;
    }

    if (button.dataset.action === "edit") {
      const title = window.prompt("Nuevo título", currentCard.title);
      if (title === null) return;
      board = editCard(board, cardId, title);
      persistAndRender("Tarjeta editada.");
      return;
    }

    if (button.dataset.action === "delete") {
      const confirmed = window.confirm(`¿Eliminar "${currentCard.title}"?`);
      if (!confirmed) return;
      board = deleteCard(board, cardId);
      persistAndRender("Tarjeta eliminada.");
    }
  } catch (error) {
    errorElement.textContent = error.message;
  }
});

searchInput.addEventListener("input", render);
columnFilter.addEventListener("change", render);

exportButton.addEventListener("click", () => {
  const content = exportBoard(board);
  const blob = new Blob([content], { type: "application/json" });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = "kanban-local.json";
  link.click();
  URL.revokeObjectURL(url);
  setStatus("Tablero exportado como JSON.");
});

importInput.addEventListener("change", async () => {
  const [file] = importInput.files;
  if (!file) return;
  errorElement.textContent = "";
  try {
    const content = await file.text();
    board = importBoard(content);
    searchInput.value = "";
    columnFilter.value = "all";
    persistAndRender(`Tablero importado: ${board.cards.length} tarjeta(s).`);
  } catch (error) {
    errorElement.textContent = error.message;
  } finally {
    importInput.value = "";
  }
});

render();

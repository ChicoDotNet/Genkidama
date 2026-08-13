import { addCard, cardsInColumn, COLUMN_IDS, moveCard } from "./board.js";
import { loadBoard, saveBoard } from "./storage.js";

const boardElement = document.querySelector("#board");
const form = document.querySelector("#new-card-form");
const titleInput = document.querySelector("#card-title");
const errorElement = document.querySelector("#form-error");
let board = loadBoard(window.localStorage);

function render() {
  for (const column of COLUMN_IDS) {
    const list = boardElement.querySelector(`[data-column="${column}"] .cards`);
    list.replaceChildren();
    for (const card of cardsInColumn(board, column)) {
      const item = document.createElement("li");
      item.className = "card";
      item.dataset.cardId = card.id;

      const text = document.createElement("span");
      text.textContent = card.title;
      item.append(text);

      const actions = document.createElement("div");
      actions.className = "card-actions";
      for (const target of COLUMN_IDS.filter((candidate) => candidate !== column)) {
        const button = document.createElement("button");
        button.type = "button";
        button.dataset.moveTo = target;
        button.textContent = `Mover a ${target}`;
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
    saveBoard(window.localStorage, board);
    form.reset();
    titleInput.focus();
    render();
  } catch (error) {
    errorElement.textContent = error.message;
  }
});

boardElement.addEventListener("click", (event) => {
  const button = event.target.closest("button[data-move-to]");
  if (!button) return;
  const card = button.closest("[data-card-id]");
  board = moveCard(board, card.dataset.cardId, button.dataset.moveTo);
  saveBoard(window.localStorage, board);
  render();
});

render();

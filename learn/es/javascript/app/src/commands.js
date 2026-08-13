import { addCard, deleteCard, editCard, moveCard } from "./board.js";

/**
 * Apply one application command to a board without introducing browser concerns.
 * @param {{cards:Array<object>}} board
 * @param {{type:string, title?:string, cardId?:string, targetColumn?:string, id?:string}} command
 * @returns {{cards:Array<object>}}
 */
export function applyBoardCommand(board, command) {
  if (!command || typeof command !== "object" || typeof command.type !== "string") {
    throw new TypeError("El comando debe declarar un tipo.");
  }

  switch (command.type) {
    case "add":
      return addCard(board, command.title, command.id);
    case "edit":
      return editCard(board, command.cardId, command.title);
    case "move":
      return moveCard(board, command.cardId, command.targetColumn);
    case "delete":
      return deleteCard(board, command.cardId);
    default:
      throw new RangeError(`Comando desconocido: ${command.type}.`);
  }
}

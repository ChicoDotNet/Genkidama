import type { CreateQuoteInput, EntityId, Quote, QuoteItem } from "./models.js";

function validateItem(item: QuoteItem): QuoteItem {
  const description = item.description.trim();
  if (description.length === 0) {
    throw new Error("Cada concepto debe tener una descripción.");
  }
  if (!Number.isFinite(item.quantity) || item.quantity <= 0) {
    throw new Error("La cantidad debe ser mayor que cero.");
  }
  if (!Number.isFinite(item.unitPrice) || item.unitPrice < 0) {
    throw new Error("El precio unitario no puede ser negativo.");
  }
  return Object.freeze({ description, quantity: item.quantity, unitPrice: item.unitPrice });
}

/**
 * Construye una cotización determinista y calcula su subtotal.
 * @throws {Error} Si falta cliente, no hay conceptos o un concepto es inválido.
 */
export function createQuote(id: EntityId, input: CreateQuoteInput): Quote {
  if (input.clientId.trim().length === 0) {
    throw new Error("La cotización requiere un cliente.");
  }
  if (input.items.length === 0) {
    throw new Error("La cotización requiere al menos un concepto.");
  }

  const items = input.items.map(validateItem);
  const subtotal = items.reduce((total, item) => total + item.quantity * item.unitPrice, 0);

  return Object.freeze({ id, clientId: input.clientId, items, subtotal });
}

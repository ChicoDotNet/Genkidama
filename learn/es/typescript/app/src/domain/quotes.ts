import type { CreateQuoteInput, EntityId, Quote, QuoteItem, QuoteQuery, QuoteStatus } from "./models.js";

const transitions: Readonly<Record<QuoteStatus, readonly QuoteStatus[]>> = {
  draft: ["sent"],
  sent: ["accepted", "rejected"],
  accepted: [],
  rejected: [],
};

function validateItem(item: QuoteItem): QuoteItem {
  const description = item.description.trim();
  if (description.length === 0) throw new Error("Cada concepto debe tener una descripción.");
  if (!Number.isFinite(item.quantity) || item.quantity <= 0) throw new Error("La cantidad debe ser mayor que cero.");
  if (!Number.isFinite(item.unitPrice) || item.unitPrice < 0) throw new Error("El precio unitario no puede ser negativo.");
  return Object.freeze({ description, quantity: item.quantity, unitPrice: item.unitPrice });
}

/** Convierte un valor externo en un estado comercial válido. */
export function parseQuoteStatus(value: unknown): QuoteStatus {
  if (value === "draft" || value === "sent" || value === "accepted" || value === "rejected") return value;
  throw new Error("Estado de cotización inválido.");
}

/**
 * Construye una cotización determinista en estado `draft` y calcula su subtotal.
 * @throws {Error} Si falta cliente, no hay conceptos o un concepto es inválido.
 */
export function createQuote(id: EntityId, input: CreateQuoteInput): Quote {
  if (input.clientId.trim().length === 0) throw new Error("La cotización requiere un cliente.");
  if (input.items.length === 0) throw new Error("La cotización requiere al menos un concepto.");

  const items = input.items.map(validateItem);
  const subtotal = items.reduce((total, item) => total + item.quantity * item.unitPrice, 0);
  return Object.freeze({ id, clientId: input.clientId, items, subtotal, status: "draft" as const });
}

/** Aplica `draft → sent → accepted|rejected` sin mutar la cotización original. */
export function changeQuoteStatus(quote: Quote, nextStatus: QuoteStatus): Quote {
  if (!transitions[quote.status].includes(nextStatus)) {
    throw new Error(`Transición de cotización no permitida: ${quote.status} → ${nextStatus}.`);
  }
  return Object.freeze({ ...quote, status: nextStatus });
}

/** Filtra cotizaciones por cliente y/o estado sin alterar la colección original. */
export function queryQuotes(quotes: readonly Quote[], query: QuoteQuery): readonly Quote[] {
  const clientId = query.clientId?.trim();
  return quotes.filter((quote) =>
    (clientId === undefined || clientId.length === 0 || quote.clientId === clientId)
    && (query.status === undefined || quote.status === query.status)
  );
}

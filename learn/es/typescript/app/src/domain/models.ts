/** Identificador estable usado por entidades de FreelanceDesk. */
export type EntityId = string;

/** Cliente administrado por el freelancer. */
export interface Client {
  readonly id: EntityId;
  readonly name: string;
  readonly email: string;
}

/** Línea cotizada con cantidad y precio unitario. */
export interface QuoteItem {
  readonly description: string;
  readonly quantity: number;
  readonly unitPrice: number;
}

/** Estado comercial permitido para una cotización. */
export type QuoteStatus = "draft" | "sent" | "accepted" | "rejected";

/** Cotización calculada para un cliente existente. */
export interface Quote {
  readonly id: EntityId;
  readonly clientId: EntityId;
  readonly items: readonly QuoteItem[];
  readonly subtotal: number;
  readonly status: QuoteStatus;
}

/** Estado permitido para un proyecto de FreelanceDesk. */
export type ProjectStatus = "planned" | "active" | "completed";

/** Proyecto asociado a un cliente y gobernado por un ciclo de vida explícito. */
export interface Project {
  readonly id: EntityId;
  readonly clientId: EntityId;
  readonly name: string;
  readonly status: ProjectStatus;
}

/** Datos externos aceptados para crear un cliente. */
export interface CreateClientInput {
  readonly name: string;
  readonly email: string;
}

/** Datos externos aceptados para crear una cotización. */
export interface CreateQuoteInput {
  readonly clientId: EntityId;
  readonly items: readonly QuoteItem[];
}

/** Datos externos aceptados para crear un proyecto. */
export interface CreateProjectInput {
  readonly clientId: EntityId;
  readonly name: string;
}

/** Cambio de estado solicitado para un proyecto existente. */
export interface ChangeProjectStatusInput {
  readonly status: ProjectStatus;
}

/** Criterios opcionales para consultar proyectos sin mutarlos. */
export interface ProjectQuery {
  readonly clientId?: EntityId;
  readonly status?: ProjectStatus;
}

/** Criterios opcionales para consultar cotizaciones sin mutarlas. */
export interface QuoteQuery {
  readonly clientId?: EntityId;
  readonly status?: QuoteStatus;
}

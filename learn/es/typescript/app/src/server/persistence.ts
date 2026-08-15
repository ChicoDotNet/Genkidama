import { mkdir, readFile, rename, writeFile } from "node:fs/promises";
import { dirname } from "node:path";
import type { Client, Project, ProjectStatus, Quote, QuoteStatus } from "../domain/models.js";

/** Estado serializable de FreelanceDesk. */
export interface AppSnapshot {
  readonly clients: readonly Client[];
  readonly quotes: readonly Quote[];
  readonly projects: readonly Project[];
}

/** Frontera de persistencia inyectable; el dominio no conoce archivos ni JSON. */
export interface AppStateStore {
  load(): Promise<AppSnapshot>;
  save(snapshot: AppSnapshot): Promise<void>;
}

const emptySnapshot: AppSnapshot = Object.freeze({ clients: [], quotes: [], projects: [] });

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

function isString(value: unknown): value is string {
  return typeof value === "string";
}

function isFiniteNumber(value: unknown): value is number {
  return typeof value === "number" && Number.isFinite(value);
}

function isProjectStatus(value: unknown): value is ProjectStatus {
  return value === "planned" || value === "active" || value === "completed";
}

function isQuoteStatus(value: unknown): value is QuoteStatus {
  return value === "draft" || value === "sent" || value === "accepted" || value === "rejected";
}

function parseClient(value: unknown): Client {
  if (!isRecord(value) || !isString(value.id) || !isString(value.name) || !isString(value.email)) {
    throw new Error("Persistencia inválida: cliente mal formado.");
  }
  return Object.freeze({ id: value.id, name: value.name, email: value.email });
}

function parseQuote(value: unknown): Quote {
  if (!isRecord(value) || !isString(value.id) || !isString(value.clientId) || !Array.isArray(value.items) || !isFiniteNumber(value.subtotal)) {
    throw new Error("Persistencia inválida: cotización mal formada.");
  }
  const items = value.items.map((item) => {
    if (!isRecord(item) || !isString(item.description) || !isFiniteNumber(item.quantity) || !isFiniteNumber(item.unitPrice)) {
      throw new Error("Persistencia inválida: concepto de cotización mal formado.");
    }
    return Object.freeze({ description: item.description, quantity: item.quantity, unitPrice: item.unitPrice });
  });
  const status = value.status === undefined ? "draft" : value.status;
  if (!isQuoteStatus(status)) throw new Error("Persistencia inválida: estado de cotización desconocido.");
  return Object.freeze({ id: value.id, clientId: value.clientId, items, subtotal: value.subtotal, status });
}

function parseProject(value: unknown): Project {
  if (!isRecord(value) || !isString(value.id) || !isString(value.clientId) || !isString(value.name) || !isProjectStatus(value.status)) {
    throw new Error("Persistencia inválida: proyecto mal formado.");
  }
  return Object.freeze({ id: value.id, clientId: value.clientId, name: value.name, status: value.status });
}

/** Convierte JSON no confiable en un snapshot validado en runtime. */
export function parseSnapshot(value: unknown): AppSnapshot {
  if (!isRecord(value) || !Array.isArray(value.clients) || !Array.isArray(value.quotes) || !Array.isArray(value.projects)) {
    throw new Error("Persistencia inválida: se esperaban clients, quotes y projects.");
  }

  return Object.freeze({
    clients: value.clients.map(parseClient),
    quotes: value.quotes.map(parseQuote),
    projects: value.projects.map(parseProject),
  });
}

/**
 * Guarda el estado en un archivo JSON mediante reemplazo atómico temporal→final.
 * Un archivo inexistente representa un estado vacío; JSON corrupto produce un error explícito.
 */
export class JsonFileStateStore implements AppStateStore {
  public constructor(private readonly filePath: string) {}

  public async load(): Promise<AppSnapshot> {
    try {
      const text = await readFile(this.filePath, "utf8");
      return parseSnapshot(JSON.parse(text) as unknown);
    } catch (error: unknown) {
      if (isRecord(error) && error.code === "ENOENT") return emptySnapshot;
      throw error;
    }
  }

  public async save(snapshot: AppSnapshot): Promise<void> {
    await mkdir(dirname(this.filePath), { recursive: true });
    const tempPath = `${this.filePath}.${process.pid}.tmp`;
    await writeFile(tempPath, `${JSON.stringify(snapshot, null, 2)}\n`, "utf8");
    await rename(tempPath, this.filePath);
  }
}

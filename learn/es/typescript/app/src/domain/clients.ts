import type { Client, CreateClientInput, EntityId } from "./models.js";

/**
 * Valida y normaliza los datos de un cliente sin realizar I/O.
 * @throws {Error} Si el nombre o correo no cumplen el contrato mínimo.
 */
export function createClient(id: EntityId, input: CreateClientInput): Client {
  const name = input.name.trim();
  const email = input.email.trim().toLowerCase();

  if (name.length < 2) {
    throw new Error("El nombre del cliente debe tener al menos 2 caracteres.");
  }

  if (!email.includes("@") || email.startsWith("@") || email.endsWith("@")) {
    throw new Error("El correo del cliente no tiene un formato válido.");
  }

  return Object.freeze({ id, name, email });
}

import type { CreateProjectInput, EntityId, Project, ProjectStatus } from "./models.js";

const transitions: Readonly<Record<ProjectStatus, readonly ProjectStatus[]>> = {
  planned: ["active"],
  active: ["completed"],
  completed: [],
};

/** Convierte un valor externo en un estado de proyecto válido. */
export function parseProjectStatus(value: unknown): ProjectStatus {
  if (value === "planned" || value === "active" || value === "completed") return value;
  throw new Error("Estado de proyecto inválido.");
}

/**
 * Crea un proyecto nuevo en estado `planned` sin realizar I/O.
 * @throws {Error} Si falta cliente o el nombre no cumple el contrato mínimo.
 */
export function createProject(id: EntityId, input: CreateProjectInput): Project {
  const clientId = input.clientId.trim();
  const name = input.name.trim();

  if (clientId.length === 0) throw new Error("El proyecto requiere un cliente.");
  if (name.length < 2) throw new Error("El nombre del proyecto debe tener al menos 2 caracteres.");

  return Object.freeze({ id, clientId, name, status: "planned" as const });
}

/**
 * Aplica una transición válida del ciclo `planned → active → completed`.
 * @throws {Error} Si la transición salta etapas o intenta reabrir un proyecto terminado.
 */
export function changeProjectStatus(project: Project, nextStatus: ProjectStatus): Project {
  if (!transitions[project.status].includes(nextStatus)) {
    throw new Error(`Transición de proyecto no permitida: ${project.status} → ${nextStatus}.`);
  }

  return Object.freeze({ ...project, status: nextStatus });
}

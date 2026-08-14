# Solución de referencia — Checkpoint 02

Una solución razonable amplía la tabla de transiciones del dominio, no el controlador HTTP:

```ts
const transitions = {
  planned: ["active"],
  active: ["planned", "completed"],
  completed: [],
} satisfies Readonly<Record<ProjectStatus, readonly ProjectStatus[]>>;
```

La prueba de dominio debe demostrar que un proyecto activo puede regresar a `planned` y que uno `completed` permanece terminal. La prueba HTTP debe ejecutar el `PATCH`, comprobar `200` y verificar que el último snapshot enviado al store contiene el nuevo estado.

No necesitas cambiar `JsonFileStateStore`: la persistencia conoce la forma de un proyecto, no las reglas que autorizan sus transiciones. Tampoco necesitas aceptar valores nuevos en `parseProjectStatus`.

Una defensa sólida explica que la tabla vive en dominio porque la regla debe ser la misma desde HTTP, una futura CLI o cualquier otro adaptador. Si después aparece `cancelled`, primero se decide qué estados pueden llegar a él y si es terminal; después se amplían tipos, regla y pruebas de forma coherente.

Vuelve a [`../lessons/08-json-confiable-y-checkpoint.md`](../lessons/08-json-confiable-y-checkpoint.md) y compara comportamiento, no nombres de variables.

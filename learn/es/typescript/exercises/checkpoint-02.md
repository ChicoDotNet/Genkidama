# Checkpoint 02 — Replanificar sin romper el historial

Trabaja sobre FreelanceDesk después de la Lección 08. No abras la solución hasta completar un intento.

## Historia

Un freelancer necesita devolver un proyecto activo a planificación cuando el cliente pospone el trabajo. Agrega la transición:

```text
active → planned
```

Conserva estas reglas:

- `planned → active` sigue permitido;
- `active → completed` sigue permitido;
- `completed` continúa siendo terminal;
- `planned → completed` sigue rechazado;
- un valor externo desconocido sigue rechazado;
- una transición inválida no debe persistir cambios.

## Evidencia mínima

Agrega al menos:

1. una prueba de dominio que demuestre `active → planned`;
2. una prueba que demuestre que `completed → planned` falla;
3. una prueba HTTP que ejecute la nueva transición y compruebe que el store recibe `planned`;
4. `npm run verify` verde.

No muevas la regla al controlador HTTP y no uses casts para aceptar JSON.

## Reflexión

Explica en 3–5 frases por qué la tabla de transiciones pertenece al dominio y qué cambiaría si después apareciera un estado `cancelled`.

Cuando termines, compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

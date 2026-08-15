# Checkpoint 01 — Prioridad crítica con una regla real

Trabaja sobre HelpDesk API después de la Lección 04. No abras la solución hasta completar un intento.

## Historia

El equipo de soporte necesita distinguir incidentes críticos, pero no quiere que cualquier ticket de una palabra pueda marcarse como crítico.

Implementa `CRITICAL` como nueva prioridad.

Reglas:

- `CRITICAL` debe ser un valor válido de `TicketPriority`.
- Un ticket crítico requiere una descripción de **al menos 20 caracteres después de normalizar espacios externos**.
- La regla pertenece al dominio; no debe depender de HTTP o Jackson.
- Tickets `LOW`, `NORMAL` y `HIGH` conservan el comportamiento actual.
- Una petición JSON crítica válida debe devolver 201.
- Una petición crítica con descripción demasiado corta debe devolver 400.
- Agrega al menos una prueba de dominio y una prueba HTTP para proteger el comportamiento.
- No cambies ni elimines pruebas existentes para conseguir verde.

## Entrega esperada

Explica brevemente:

1. dónde colocaste la regla;
2. por qué no la implementaste sólo en `HelpDeskHttpServer`;
3. qué prueba falló primero;
4. cómo verificaste que prioridades anteriores no cambiaron.

## Comprobación

```bash
mvn verify
```

Después prueba manualmente:

```json
{
  "title": "Producción caída",
  "description": "Servicio principal no responde",
  "priority": "CRITICAL"
}
```

Y un caso crítico cuya descripción tenga menos de 20 caracteres.

Cuando termines, compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

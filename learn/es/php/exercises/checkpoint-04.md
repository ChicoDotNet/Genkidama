# Checkpoint 04 — Rechazar antes de mutar

## Objetivo

Demostrar que comprendes la frontera de seguridad HTTP de AgendaPHP y puedes extenderla sin mover reglas de dominio.

## Encargo

Añade una nueva mutación sencilla llamada `duplicate` que, dado el ID de una cita existente, cree otra cita 7 días después con un ID nuevo y los mismos cliente, servicio y duración.

La operación sólo debe ejecutarse cuando:

1. el request sea `application/x-www-form-urlencoded`;
2. el body esté dentro del límite actual;
3. el token CSRF sea válido;
4. la nueva cita no choque con otra cita.

## Restricciones

- No desactives ni rodees la protección CSRF.
- La regla de “7 días después” pertenece a la aplicación, no a la persistencia.
- Si falla la validación o el almacenamiento, el calendario durable anterior debe permanecer consistente.
- Agrega una regresión HTTP que pruebe que `duplicate` **sin token** devuelve 403 y no crea ninguna cita.
- Agrega al menos una prueba funcional para el camino exitoso.

## Cómo comprobar

```bash
cd app
bash tools/verify.sh
bash tools/smoke.sh
```

## Criterios de terminado

- [ ] La nueva mutación respeta todas las defensas HTTP existentes.
- [ ] El estado sólo cambia después de validar la cita candidata.
- [ ] Existe una regresión para el rechazo CSRF.
- [ ] Existe una prueba del camino exitoso.
- [ ] Los gates existentes siguen verdes sin relajarlos.

Intenta resolverlo antes de consultar la [solución de referencia](../solutions/checkpoint-04.md).

# Solución de referencia — Checkpoint 04

Una solución defendible mantiene dos responsabilidades separadas.

## 1. La frontera HTTP sigue rechazando primero

`duplicate` debe entrar por el mismo bloque POST que `book`, `update` y `cancel`. No necesita una excepción especial: primero pasan límite de body, media type y `hash_equals` del token CSRF; sólo después se interpreta `action`.

La regresión HTTP debe enviar `action=duplicate` e `id=<existente>` **sin `csrfToken`**, esperar 403 y verificar que el documento JSON conserva exactamente el mismo número de citas.

## 2. La aplicación construye la cita candidata

Una implementación limpia agrega un método a `AppointmentService`, por ejemplo `duplicateOneWeekLater(string $id)`. El método:

1. carga el `Schedule` actual;
2. localiza la cita por ID;
3. crea una nueva cita usando el generador de IDs existente;
4. desplaza `startsAt` exactamente `+7 days`;
5. intenta agregarla al calendario candidato;
6. persiste sólo cuando todas las reglas pasan.

La frontera HTTP no debe reimplementar detección de cruces ni manipular JSON.

## Pruebas útiles

- camino feliz: nueva cita con otro ID y fecha +7 días;
- conflicto en la nueva fecha: `DomainException` y estado durable sin cambios;
- POST sin CSRF: 403 y ninguna mutación;
- fallo de store: no presentar la cita como durable si `save()` falla.

## Qué no copiar ciegamente

El nombre exacto del método no importa. La solución se evalúa por contratos: rechazo temprano, reglas centralizadas, persistencia candidata y regresiones significativas.

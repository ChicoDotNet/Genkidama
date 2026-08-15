# Checkpoint 02 — Escalamiento persistente

Trabaja sobre HelpDesk sin abrir la solución hasta terminar un intento.

## Escenario

Soporte necesita escalar tickets sin perder el estado confirmado si el almacenamiento falla.

Implementa una operación `escalate(long id)` con estas reglas:

- `LOW → NORMAL`;
- `NORMAL → HIGH`;
- `HIGH` permanece `HIGH`;
- no cambia título, descripción ni estado;
- un ID inexistente conserva el error explícito actual;
- la mutación debe persistirse antes de hacerse visible;
- si el store falla, el ticket visible conserva su prioridad anterior.

Expón la operación mediante un endpoint específico que no permita modificar campos arbitrarios.

## Pruebas mínimas

Añade al menos:

1. escalamiento LOW → NORMAL → HIGH;
2. HIGH idempotente;
3. estado de ciclo de vida preservado;
4. store fallido no publica el cambio;
5. regresión HTTP representativa.

## Comprobación

```bash
cd app
mvn verify
```

Después ejecuta la API con `HELPDESK_DATA_FILE` apuntando a un archivo temporal, escala un ticket, reinicia el proceso y comprueba que la prioridad persistió.

## Reflexión

Explica en 3–5 frases por qué `escalate` pertenece al servicio de dominio/aplicación y no a `JsonFileTicketStore` ni a `HelpDeskHttpServer`.

Cuando termines, compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

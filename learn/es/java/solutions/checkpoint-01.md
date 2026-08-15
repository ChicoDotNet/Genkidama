# Solución de referencia — Checkpoint 01

> Consulta esta referencia sólo después de intentar el checkpoint. No existe una única distribución correcta de líneas.

## Dirección de diseño

La prioridad es parte del dominio, así que `TicketPriority` puede incorporar `CRITICAL`. La restricción de descripción crítica debe evaluarse en la misma frontera de dominio que normaliza y valida los datos antes de construir `Ticket`.

Una implementación razonable, dentro de `TicketService.create()`, primero normaliza descripción y prioridad y después aplica una regla equivalente a:

```java
if (normalizedPriority == TicketPriority.CRITICAL
        && normalizedDescription.length() < 20) {
    throw new IllegalArgumentException(
            "critical tickets require at least 20 description characters");
}
```

No copies el fragmento si tu diseño expresa la regla de forma igualmente clara en una función privada con nombre.

## Prueba de dominio esperada

Protege al menos:

- `CRITICAL` con descripción de 20+ caracteres crea ticket;
- `CRITICAL` con descripción corta lanza `IllegalArgumentException`;
- otra prioridad con descripción corta sigue permitida.

## Prueba HTTP esperada

Envía JSON real al servidor:

- caso crítico válido → `201` y prioridad `CRITICAL` en respuesta;
- caso crítico inválido → `400`;
- el HTTP adapter no repite la regla de longitud.

## Por qué esta solución

La regla sobreviviría aunque mañana HelpDesk tuviera CLI, mensajería o Spring MVC. El protocolo sólo traduce el error del dominio a 400.

## Qué no hacer

- validar los 20 caracteres únicamente en JavaScript/curl/HTTP;
- convertir una descripción ausente en una excepción distinta sólo para el caso HTTP;
- atrapar el error y continuar creando el ticket;
- modificar la regla para que una prueba existente “pase”.

Vuelve a la [Lección 04](../lessons/04-errores-y-checkpoint.md) y explica la decisión sin mirar código.

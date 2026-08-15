# Solución de referencia — Checkpoint 02

No existe una única solución correcta. Compara comportamiento y fronteras, no nombres exactos.

Una dirección razonable es mantener `escalate` junto a `advance` y `changePriority` en `TicketService`:

```java
public synchronized Ticket escalate(long id) {
    Ticket current = get(id);
    TicketPriority next = switch (current.priority()) {
        case LOW -> TicketPriority.NORMAL;
        case NORMAL, HIGH -> TicketPriority.HIGH;
    };
    if (next == current.priority()) {
        return current;
    }
    return changePriority(id, next);
}
```

La solución puede reutilizar `changePriority` porque esa operación ya conserva identidad/estado y persiste el candidato antes de publicarlo. Otra implementación es válida si mantiene esas garantías y evita duplicar la política de persistencia.

El endpoint puede ser, por ejemplo, `POST /api/tickets/{id}/escalate`. HTTP sólo traduce la intención; no decide qué prioridad sigue.

Una prueba de fallo debe usar un `TicketStore` controlado: cargar un ticket conocido, configurar el store para fallar en `save`, ejecutar `escalate` y demostrar que `get(id)` conserva la prioridad previa.

La idea central del checkpoint es distinguir tres responsabilidades:

- **dominio/servicio:** qué significa escalar;
- **persistencia:** guardar o fallar explícitamente;
- **HTTP:** representar la operación y sus resultados.

Si tu solución mezcla esas decisiones, refactoriza antes de considerarla terminada.

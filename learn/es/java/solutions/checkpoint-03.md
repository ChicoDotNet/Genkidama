# Solución de referencia — Checkpoint 03

Una solución razonable crea el fallo en la frontera de persistencia, no dentro del HTTP handler.

## Store controlado

```java
final class FailingStore implements TicketStore {
    @Override
    public List<Ticket> load() {
        return List.of();
    }

    @Override
    public void save(List<Ticket> tickets) {
        throw new TicketPersistenceException("simulated failure");
    }
}
```

Arranca `HelpDeskHttpServer` con `new TicketService(new FailingStore())` y diagnóstico habilitado. Envía un POST con un título reconocible sólo para poder comprobar después que no aparece en el JSON diagnóstico.

## Comprobaciones esperadas

- El POST devuelve `503`.
- `/api/tickets/summary` mantiene `total == 0` porque el candidato nunca fue publicado.
- `/api/diagnostics` informa al menos una respuesta 5xx mediante `failures`.
- Serializar el diagnóstico y buscar el título/descripción del request debe dar falso.

La prueba no necesita saber cómo `RequestMetrics` almacena sus contadores. Protege comportamiento público.

## Por qué importa

El 503 es una señal operacional: el servidor no pudo completar una dependencia propia. El contador permite detectar frecuencia de ese tipo de fallo sin convertir métricas en un archivo de auditoría con contenido potencialmente sensible.

Una auditoría completa tendría otros requisitos —identidad, trazabilidad, retención, integridad y acceso— que este curso no pretende resolver.

## Reto de latencia

En lugar de guardar cada duración, una frontera de métricas podría incrementar contadores por bucket. Eso conserva agregación y limita datos almacenados. Conviene inyectar un reloj o duración ya calculada para probar los límites sin depender de `sleep`.

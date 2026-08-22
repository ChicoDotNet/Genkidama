# Solución de referencia — Checkpoint 04

La intención es demostrar límites y observabilidad, no memorizar una implementación única.

Para la paginación, una prueba razonable crea más de `ContactsController::PAGE_SIZE` contactos, solicita `page=2` y comprueba el texto de página/conteo. Para `page=0`, el contrato esperado es normalizar a página 1; la prueba debe afirmar `Página 1` y no depender del orden de IDs.

`bin/rails contactdesk:diagnostics` debe producir sólo estado, conexión, conteo y timestamp. No necesita recorrer contactos: `Contact.count` deja claro que la señal operativa no requiere PII.

`/healthz` reutiliza el mismo servicio y añade `request_id`; ese identificador permite correlacionar una respuesta concreta con sus logs sin mandar un stack trace al cliente.

Con millones de filas, el siguiente paso no sería “más Ruby”: revisaríamos índices, costo de `COUNT`, paginación por cursor/keyset, perfiles de consulta y requerimientos reales antes de escoger una solución.

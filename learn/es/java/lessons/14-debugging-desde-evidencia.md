# Lección 14 — Debugging desde evidencia

## Qué vas a conseguir

Vas a depurar HelpDesk siguiendo la ruta observable `request → adaptación HTTP → servicio → persistencia` en lugar de editar al azar.

## Antes de empezar

Completa la [Lección 13](13-gate-profesional-y-contratos-http.md).

## El problema

Un `400`, `409`, `503` y `500` pueden parecer simplemente “la API falló”, pero significan clases de problema diferentes. Si no reduces el espacio de búsqueda, el debugging se vuelve ensayo y error.

## Concepto

Depurar es formular una hipótesis y buscar evidencia que pueda refutarla. Empieza por la frontera más cercana al síntoma:

1. status HTTP y respuesta;
2. validación/transporte;
3. excepción de dominio;
4. persistencia;
5. sólo después detalles internos.

## Demostración

[DEMO] Ejecuta una transición inválida. El `409` indica que el request llegó al dominio. Después simula un `TicketStore` que falla: `503` indica que la frontera de persistencia no pudo confirmar el snapshot.

## Código real

`HelpDeskHttpServer` traduce excepciones específicas a estados específicos. Esta separación permite localizar la capa afectada sin imprimir el ticket completo ni registrar PII.

## Qué acaba de pasar

Los errores explícitos son una herramienta de diagnóstico, no sólo una mejora estética de API.

## Errores comunes

- Atrapar `Exception` y responder siempre `500`.
- Imprimir bodies para “ver qué pasó”.
- Cambiar dominio, HTTP y persistencia al mismo tiempo.
- Confundir un síntoma de transporte con una regla de negocio.

## Buenas prácticas

Reduce el problema con la prueba más pequeña que reproduzca el síntoma. Conserva la regresión cuando encuentres la causa.

## Tu turno

[PAUSA PARA EJERCICIO] Provoca una prioridad desconocida, identifica la capa que la rechaza y escribe una explicación de por qué debe ser `400`, no `409`.

## Cómo comprobar

```bash
mvn -Dtest=HelpDeskHttpServerTest test
mvn verify
```

## Solución enlazada

La suite existente sirve como referencia después de tu intento.

## Reto adicional

Diseña qué información incluirías en logs de producción sin registrar título, descripción ni body del ticket.

## Resumen

- Un status bien elegido reduce el espacio de búsqueda.
- La excepción específica conserva contexto técnico.
- La regresión evita repetir el incidente.
- Debugging no justifica exponer datos sensibles.

## Siguiente paso

Continúa con [Lección 15 — Medir antes de optimizar](15-medir-antes-de-optimizar.md).

## Referencias

- [Java Exceptions — Java Tutorials](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- [JUnit User Guide](https://docs.junit.org/6.1.2/)

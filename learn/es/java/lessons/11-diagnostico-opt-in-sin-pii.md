# Lección 11 — Diagnóstico opt-in sin PII

## Qué vas a conseguir

Vas a añadir diagnóstico agregado para saber si el servidor está recibiendo peticiones y produciendo errores 5xx sin guardar URLs, cuerpos, títulos, descripciones ni IDs de tickets.

## Antes de empezar

Completa la [Lección 10](10-concurrencia-y-executor.md).

## El problema

Cuando una API falla, “no sé qué está ocurriendo” es caro. Pero registrar cada request completo también puede crear un problema: tickets de soporte suelen contener nombres, correos, incidentes internos o datos sensibles.

## Concepto

`RequestMetrics` mantiene únicamente dos contadores thread-safe mediante `LongAdder`:

- respuestas completadas;
- respuestas 5xx.

Los diagnósticos son **opt-in**. Sólo si `HELPDESK_DIAGNOSTICS=1` o `true` se publica `/api/diagnostics`.

## Demostración

[EJECUTAR]

```bash
HELPDESK_DIAGNOSTICS=1 mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
curl http://localhost:8080/health
curl http://localhost:8080/api/diagnostics
```

La respuesta contiene agregados de requests y el `TicketSummary`; no contiene rutas visitadas ni contenido de tickets.

## Código real

La métrica recibe sólo el resultado final:

```java
public void record(int statusCode) {
    requests.increment();
    if (statusCode >= 500) failures.increment();
}
```

No existe un parámetro para body, URL o usuario. Reducir la superficie disponible también reduce la posibilidad de registrar PII por accidente.

## Qué acaba de pasar

HelpDesk ganó observabilidad suficiente para prácticas operativas básicas manteniendo el diagnóstico separado del dominio y desactivado por defecto.

## Errores comunes

- Loggear request/response completos “por si acaso”.
- Incluir títulos o descripciones de tickets en métricas.
- Mezclar contadores operativos con reglas de negocio.
- Exponer diagnóstico detallado sin una decisión explícita de producto/seguridad.
- Considerar cualquier 4xx como fallo del servidor.

## Buenas prácticas

Recoge la menor cantidad de datos que responda una pregunta operativa concreta. Separa señal agregada de trazas de contenido. Documenta claramente qué está habilitado y qué no.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una prueba que provoque un 503 mediante un `TicketStore` fallido y comprueba que `failures` aumenta sin inspeccionar el cuerpo del ticket.

## Cómo comprobar

```bash
mvn verify
```

Además ejecuta una instancia con diagnóstico desactivado y confirma que `/api/diagnostics` no forma parte de su superficie pública.

## Solución enlazada

La suite HTTP demuestra la ruta opt-in y los contadores agregados. Intenta primero tu propia prueba negativa.

## Reto adicional

Diseña qué métricas añadirías para latencia sin almacenar PII. Explica por qué un histograma suele aportar más que guardar cada duración individual.

## Resumen

- Diagnóstico útil no exige capturar contenido sensible.
- `LongAdder` permite contadores concurrentes simples.
- La ruta es opt-in y pertenece a la frontera HTTP.
- Las métricas 5xx no sustituyen logs estructurados ni tracing cuando una aplicación crece.

## Siguiente paso

En la [Lección 12](12-operacion-confiable-y-checkpoint.md) integrarás resumen, concurrencia y diagnóstico en un checkpoint operativo.

## Referencias

- [LongAdder — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/concurrent/atomic/LongAdder.html)
- [Java Secure Coding Guidelines](https://www.oracle.com/java/technologies/javase/seccodeguide.html)

# Lección 15 — Medir antes de optimizar

## Qué vas a conseguir

Vas a añadir una señal de duración agregada y determinista para razonar sobre rendimiento sin convertir HelpDesk en una plataforma de observabilidad.

## Antes de empezar

Completa la [Lección 14](14-debugging-desde-evidencia.md).

## El problema

“Se siente lento” no identifica qué optimizar. Cambiar streams, locks o pools sin una medición puede aumentar complejidad y no mover el cuello de botella real.

## Concepto

Mide antes de optimizar. Para duración dentro de un proceso usa un reloj monotónico (`System.nanoTime`), no hora civil. HelpDesk acumula duración total, solicitudes y fallas; no retiene la duración de cada request ni sus datos.

## Demostración

[DEMO] Con diagnóstico habilitado consulta `/api/diagnostics` después de varias peticiones. Observa `requests`, `failures` y `totalDurationNanos`.

La prueba inyecta un `LongSupplier` controlado. Así comprueba exactamente la medición sin sleeps ni límites de tiempo frágiles.

## Código real

La medición envuelve al handler HTTP:

```java
long started = nanoTime.getAsLong();
try {
    handler.handle(exchange);
} finally {
    metrics.recordDuration(Math.max(0L, nanoTime.getAsLong() - started));
}
```

El dominio no conoce el reloj.

## Qué acaba de pasar

HelpDesk obtiene una señal suficiente para formular preguntas sin adoptar prematuramente tracing distribuido ni almacenar datos por request.

## Errores comunes

- Usar `currentTimeMillis()` para duración.
- Escribir tests de performance que dependen de `sleep`.
- Optimizar el código con más líneas porque una operación “parece” costosa.
- Confundir una métrica agregada educativa con un benchmark riguroso.

## Buenas prácticas

Mide una línea base, cambia una cosa y vuelve a medir. Para análisis serio usa herramientas como Java Flight Recorder/JDK Mission Control en vez de inventar un profiler casero.

## Tu turno

[PAUSA PARA EJERCICIO] Calcula fuera del servidor la duración media aproximada usando `totalDurationNanos / requests`. Explica por qué no conviene persistir ese promedio como fuente de verdad.

## Cómo comprobar

```bash
mvn verify
```

## Solución enlazada

La prueba `diagnosticsAreOptInAggregateAndUseMonotonicTiming` muestra una verificación determinista.

## Reto adicional

Describe qué medirías antes de aumentar `HTTP_WORKERS` de 4 a 8.

## Resumen

- Primero evidencia, después optimización.
- `nanoTime` sirve para intervalos monotónicos.
- El reloj se inyecta sólo donde mejora testabilidad.
- El diagnóstico sigue agregado y sin PII.

## Siguiente paso

Continúa con [Lección 16 — Hardening y Checkpoint 04](16-hardening-y-checkpoint-04.md).

## Referencias

- [System.nanoTime — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/System.html#nanoTime())
- [Java Flight Recorder](https://docs.oracle.com/en/java/javase/25/jfapi/)

# Lección 15 — Diagnóstico y rendimiento con evidencia

## Qué vas a conseguir

Añadirás observabilidad mínima y opt-in para medir antes de optimizar, sin convertir logs en una fuga de datos.

## Antes de empezar

Completa la [Lección 14](14-tooling-y-gate-profesional.md).

## El problema

“Se siente lento” no identifica qué optimizar. Registrar cada URL, body o cliente tampoco es una solución profesional: puede introducir datos personales y ruido antes de tener una pregunta diagnóstica.

## Concepto

`RequestMetrics` agrega únicamente cuatro señales: peticiones totales, peticiones con status >=400, duración acumulada y duración máxima. No registra rutas, IDs, correos ni payloads. Además, el endpoint sólo existe cuando `FREELANCEDESK_DIAGNOSTICS=1` habilita explícitamente el diagnóstico.

## Demostración

[EN PANTALLA] Inicia normalmente y prueba `/api/diagnostics`: obtendrás 404. Después ejecuta:

```bash
FREELANCEDESK_DIAGNOSTICS=1 npm start
```

Haz algunas peticiones y consulta `/api/diagnostics`.

## Código real

`app/src/server/diagnostics.ts` mantiene el agregado; el handler recibe tanto métricas como reloj por inyección. Las pruebas usan un reloj determinista, de modo que no dependen de milisegundos reales del runner.

## Qué acaba de pasar

Ahora existe evidencia para preguntar “¿cuántas peticiones fallan?” o “¿cuál fue la duración máxima observada?” sin almacenar contenido sensible. Todavía no sabemos *por qué* algo tarda; la métrica señala dónde investigar después.

## Errores comunes

- Optimizar una consulta sin haber medido que sea relevante.
- Registrar cuerpos completos para “debug”.
- Tratar una medición local como garantía de producción.
- Hacer que una prueba de tiempo dependa del reloj real.

## Buenas prácticas

Mide lo mínimo que responda una pregunta. Inyecta reloj cuando la lógica debe ser determinista y evita observabilidad que capture información que no necesitas.

## Tu turno

[PAUSA PARA EJERCICIO] Explica qué métrica adicional necesitarías para distinguir latencia de persistencia frente a latencia de render, sin implementarla todavía.

## Cómo comprobar

```bash
npm run verify
```

La suite debe demostrar que el diagnóstico está apagado por defecto y que, habilitado, agrega resultados deterministas.

## Solución enlazada

Revisa `app/src/server/diagnostics.ts` sólo después de proponer qué datos mínimos guardarías.

## Reto adicional

Diseña percentiles de latencia sin guardar para siempre cada duración. Compara exactitud, memoria y complejidad antes de escribir código.

## Resumen

Rendimiento profesional empieza con evidencia. Observabilidad profesional también exige decidir qué **no** recolectar.

## Siguiente paso

Continúa con la [Lección 16 — Hardening HTTP y Checkpoint 04](16-hardening-http-y-checkpoint-04.md).

## Referencias

- [Node.js Performance Measurement APIs](https://nodejs.org/api/perf_hooks.html)
- [OWASP Logging Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Logging_Cheat_Sheet.html)

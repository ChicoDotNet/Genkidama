# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir

Vas a demostrar que puedes leer, modificar, probar y explicar Kanban Local sin un tutorial paso a paso. Esta lección no introduce un concepto principal nuevo: integra lo aprendido.

## Antes de empezar

Desde `app/`:

```bash
npm run verify
npm start
```

Abre la aplicación y confirma que puedes crear, mover, editar, buscar, exportar e importar tarjetas. Haz también la comprobación manual online → offline → recarga descrita durante el curso.

## El problema

Un cliente usa Kanban Local para trabajo real y pide una evolución pequeña, pero exige conservar compatibilidad, seguridad de importación y operación offline. No recibirás una lista de archivos o líneas que debas cambiar.

## Concepto

Una evaluación profesional no mide memoria de sintaxis. Mide si puedes **leer → formular → probar → implementar → diagnosticar → verificar → explicar**.

## Demostración

[DEMO] Antes de modificar nada, recorre `src/app.js`, `src/commands.js`, el dominio y los adaptadores de persistencia. Explica en voz alta dónde colocarías una regla nueva y por qué.

## Código real

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md). Implementa el encargo sobre la misma aplicación canónica. Puedes consultar documentación oficial, las lecciones, mensajes de error y las herramientas del navegador.

No abras la solución antes de completar un intento.

## Qué acaba de pasar

Ya no estás siguiendo una receta: estás trabajando sobre una base existente, conservando contratos y decidiendo la frontera correcta para cada cambio.

## Errores comunes

- Cambiar el DOM para resolver una regla que pertenece al dominio o a comandos.
- Confiar en la extensión o el MIME de un archivo importado sin validar contenido.
- Convertir un requisito de búsqueda en estado persistido innecesario.
- Corregir un bug sin una prueba de regresión.
- Declarar que una PWA funciona offline porque `navigator.onLine` devuelve un valor.
- Optimizar antes de medir.

## Buenas prácticas

Mantén funciones pequeñas, errores útiles, validación en fronteras, reglas deterministas fuera del DOM y pruebas que protejan comportamiento. Conserva `npm run verify` como contrato local/CI.

## Tu turno

[PAUSA PARA EJERCICIO] Completa las historias A–E de la evaluación. Después prepara una explicación de cinco minutos sobre arquitectura, seguridad, persistencia, diagnóstico y un tradeoff que hayas aceptado.

## Cómo comprobar

Como mínimo:

```bash
npm run verify
npm start
```

Además, prueba manualmente el flujo nuevo, una entrada inválida, export/import y una recarga offline. Usa la [`rúbrica final`](../exercises/rubrica-final.md) para autoevaluarte.

## Solución enlazada

Sólo después de tu intento, compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). La referencia muestra una dirección posible; no exige que tu código sea idéntico.

## Reto adicional

Explica qué cambiaría si Kanban Local pasara de un único navegador a múltiples usuarios sincronizados. No implementes backend: identifica contratos, conflictos y riesgos.

## Cómo hablar de este proyecto en una entrevista

Cuenta primero el problema y después las decisiones: JavaScript nativo para dominar plataforma, dominio separado del DOM, IndexedDB con compatibilidad, service worker/app shell, comandos testeables, importación tratada como frontera y diagnóstico opt-in. Menciona una limitación real; por ejemplo, una PWA local no resuelve sincronización multiusuario.

Preguntas probables:

- ¿Por qué separaste dominio, comandos y adaptadores?
- ¿Qué diferencia hay entre `localStorage` e IndexedDB aquí?
- ¿Qué puede y qué no puede demostrar `navigator.onLine`?
- ¿Cómo evitas que una importación inválida destruya el tablero?
- ¿Qué mide tu diagnóstico y cuándo optimizarías?
- ¿Qué cambiarías para sincronizar varios dispositivos?

## Resumen

Completar el curso significa poder modificar una aplicación real con pruebas y explicar las decisiones, no haber leído 17 documentos. La evaluación produce evidencia concreta de competencia inicial; no garantiza contratación.

## Siguiente paso

Repite las áreas débiles de la rúbrica, conserva el proyecto como evidencia y construye una variante propia sin copiar la solución.

## Referencias

- [JavaScript — MDN](https://developer.mozilla.org/docs/Web/JavaScript)
- [`node:test`](https://nodejs.org/api/test.html)
- [IndexedDB — MDN](https://developer.mozilla.org/docs/Web/API/IndexedDB_API)
- [Service Worker API — MDN](https://developer.mozilla.org/docs/Web/API/Service_Worker_API)
- [Content Security Policy — MDN](https://developer.mozilla.org/docs/Web/HTTP/CSP)

# Lección 16 — Hardening HTTP y Checkpoint 04

## Qué vas a conseguir

Cerrarás el bloque profesional limitando entradas, declarando formato y añadiendo headers defensivos sin alterar las reglas del dominio.

## Antes de empezar

Completa la [Lección 15](15-diagnostico-y-rendimiento-con-evidencia.md).

## El problema

Un servidor que acumula un body sin límite permite que una petición consuma memoria innecesaria. Un consumidor que envía JSON como `text/plain` vuelve ambiguo el contrato. Y una app web servida sin headers defensivos deja decisiones del navegador implícitas.

## Concepto

FreelanceDesk aplica controles pequeños en la frontera HTTP:

- `Content-Type: application/json` obligatorio para cuerpos JSON;
- límite de 64 KiB por defecto, con `413 Payload Too Large`;
- `X-Content-Type-Options: nosniff`;
- `Referrer-Policy: no-referrer`;
- una CSP mínima para recursos propios y sin objetos/frame ancestors.

Estos controles reducen superficie; no sustituyen autenticación, autorización, TLS ni una revisión de seguridad completa.

## Demostración

[DEMO] Revisa `readJson` y `applySecurityHeaders`. Una petición rechazada por media type o tamaño ocurre antes de construir un snapshot candidato, por lo que no llega a persistencia.

## Código real

`app/tests/operational.test.ts` protege los contratos `415`, `413`, headers y diagnóstico opt-in. La prueba del límite usa un valor pequeño inyectado para ser rápida y determinista.

## Qué acaba de pasar

La app ya no acepta bytes ilimitados ni formatos ambiguos y el navegador recibe políticas explícitas. Las defensas permanecen en el adaptador HTTP; los módulos de clientes, proyectos y cotizaciones no cambian.

## Errores comunes

- Suponer que CSP reemplaza escape/validación.
- Llamar “segura” a una app sólo por añadir headers.
- Leer todo el body y validar tamaño al final.
- Persistir parcialmente antes de descubrir que una entrada era inválida.

## Buenas prácticas

Falla temprano en fronteras, limita recursos y prueba que el rechazo no muta estado. Documenta también qué amenazas **no** cubre tu control.

## Tu turno — Checkpoint 04

Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución. Extenderás el diagnóstico agregado para distinguir errores del servidor sin registrar información sensible.

## Cómo comprobar

```bash
npm run verify
```

Debes conservar todas las regresiones anteriores y las nuevas pruebas operativas.

## Solución enlazada

Después de tu intento consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).

## Reto adicional

Diseña una política de límites distinta para un endpoint futuro de archivos. Explica por qué no reutilizarías ciegamente el límite de JSON.

## Resumen

Hardening útil es específico, verificable y honesto sobre sus límites. La seguridad sigue siendo una propiedad de varias capas.

## Siguiente paso

Continúa con la [Lección 17 — Evaluación final sin receta](17-evaluacion-final.md), donde integrarás el curso completo sin una guía paso a paso.

## Referencias

- [HTTP 413 — MDN](https://developer.mozilla.org/docs/Web/HTTP/Status/413)
- [HTTP 415 — MDN](https://developer.mozilla.org/docs/Web/HTTP/Status/415)
- [Content Security Policy — MDN](https://developer.mozilla.org/docs/Web/HTTP/CSP)
- [OWASP HTTP Headers Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Headers_Cheat_Sheet.html)

# Lección 16 — Hardening HTTP + Checkpoint 04

## Qué vas a conseguir

Vas a proteger las mutaciones de AgendaPHP contra requests no verificadas, limitar entradas HTTP y cerrar el cuarto checkpoint con una regresión real.

## Antes de empezar

Completa la [Lección 15](15-medir-antes-de-optimizar.md).

## El problema

Un botón “Cancelar cita” usa POST, pero POST por sí solo no demuestra que la solicitud se originó desde el formulario que AgendaPHP entregó. Un sitio externo podría intentar provocar una mutación desde el navegador de una persona que ya tiene sesión.

## Concepto

AgendaPHP usa un token CSRF aleatorio guardado en sesión. Cada formulario mutador lo envía como campo oculto y el servidor compara con `hash_equals` **antes** de llamar a la aplicación.

La misma frontera aplica tres defensas adicionales:

- máximo de 16 KiB para el body de una mutación;
- `application/x-www-form-urlencoded` como media type explícito;
- headers `nosniff`, `Referrer-Policy`, CSP y `Cache-Control: no-store`.

La cookie de sesión es `HttpOnly`, `SameSite=Lax` y usa `Secure` cuando el request llega por HTTPS.

## Demostración

[EJECUTAR]

```bash
cd app
bash tools/smoke.sh
```

El smoke abre primero la página para recibir cookie + token. Después intenta crear una cita **sin token**: espera 403 y confirma que el archivo durable no existe. También intenta JSON y espera 415. Finalmente recorre create → edit → query → CSV → cancel con el token válido.

## Código real

Consulta [`public/index.php`](../app/public/index.php) y [`tools/smoke.sh`](../app/tools/smoke.sh). La protección vive en la frontera HTTP; `AppointmentService` conserva exactamente las mismas reglas de negocio.

## Qué acaba de pasar

El orden importa: una solicitud no confiable se rechaza antes de persistencia. No añadimos “seguridad” como texto en README; añadimos comportamiento ejecutable y una regresión que fallaría si alguien retirara el token.

## Errores comunes

- Usar un token constante o predecible.
- Aceptar token por query string y filtrarlo en URLs/logs.
- Creer que `SameSite` reemplaza una defensa CSRF explícita.
- Añadir CSP y afirmar que la app ya es segura para Internet.
- Proteger crear/editar pero olvidar cancelar.

## Buenas prácticas

Seguridad es defensa por capas. Este curso sigue siendo una aplicación local educativa: no incorpora identidad, autorización, TLS terminado por la app, rate limiting ni gestión de secretos. Los headers y CSRF reducen riesgos concretos; no eliminan todos.

## Tu turno — Checkpoint 04

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución.

## Cómo comprobar

```bash
bash tools/verify.sh
bash tools/smoke.sh
```

## Solución enlazada

Consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md) después de intentar el ejercicio.

## Reto adicional

Diseña cómo rotarías el token después de una autenticación futura sin romper un formulario ya abierto. Explica el trade-off.

## Resumen

- Toda mutación exige token CSRF ligado a sesión.
- El rechazo ocurre antes de llamar al dominio/persistencia.
- El contrato HTTP limita media type y tamaño.
- Los headers defensivos complementan, no sustituyen, controles de identidad/autorización/TLS.

## Siguiente paso

Continúa con la [Lección 17 — Evaluación final sin receta](17-evaluacion-final.md) para demostrar que puedes mantener AgendaPHP sin instrucciones paso a paso.

## Referencias

- [PHP — Sessions](https://www.php.net/manual/en/book.session.php)
- [PHP — random_bytes](https://www.php.net/manual/en/function.random-bytes.php)
- [PHP — hash_equals](https://www.php.net/manual/en/function.hash-equals.php)
- [OWASP CSRF Prevention Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html)
- [Content-Security-Policy — MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy)

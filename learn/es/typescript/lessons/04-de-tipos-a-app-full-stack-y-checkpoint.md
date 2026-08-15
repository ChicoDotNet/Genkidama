# Lección 04 — De tipos a una aplicación full-stack y Checkpoint 01

## Qué vas a conseguir

Ejecutarás FreelanceDesk de extremo a extremo: navegador → API Node.js → reglas TypeScript → respuesta JSON. Cerrarás el primer checkpoint modificando una regla sin romper la frontera HTTP.

## Antes de empezar

Completa la [Lección 03](03-funciones-modulos-y-validacion.md).

## El problema

Una regla aislada no demuestra que una aplicación funcione. Necesitamos comprobar que los mismos contratos sobreviven al cruzar HTTP y que los errores no crean datos parciales.

## Concepto

Full-stack no significa “usar muchos frameworks”. Significa trabajar en más de una capa de ejecución. Aquí Node.js expone API y archivos estáticos; el navegador usa `fetch`; el dominio permanece independiente de ambos. El estado en memoria es una limitación consciente: permite enseñar primero tipos, HTTP y pruebas sin introducir persistencia antes de que exista un problema visible.

## Demostración

[EJECUTAR]

```bash
npm start
```

Abre `http://localhost:3000`, crea un cliente y después una cotización. Observa el subtotal en la UI. Reinicia el servidor y confirma que los datos desaparecen: no lo escondemos.

## Código real

`createRequestHandler` recibe un `AppState` inyectado. Eso permite que cada prueba HTTP tenga estado independiente. El navegador importa tipos con `import type`, pero los datos reales siguen llegando como JSON.

## Qué acaba de pasar

La aplicación ya cruza tres fronteras sin mezclar responsabilidades: DOM, HTTP y dominio.

## Errores comunes

- Hacer que el dominio conozca `Request`, `Response` o elementos HTML.
- Compartir estado global entre pruebas.
- Confiar en `type="email"` como única defensa del servidor.
- Añadir base de datos sólo para poder decir “full-stack”.

## Buenas prácticas

Inyecta dependencias simples, prueba respuestas reales y conserva la regla de negocio fuera del transporte.

## Tu turno — Checkpoint 01

Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) sin abrir la solución. El cambio exige agregar descuento porcentual opcional a una cotización manteniendo compatibilidad con las llamadas actuales.

## Cómo comprobar

```bash
npm run verify
npm start
```

Además prueba manualmente un cliente válido, una cotización válida y una cotización para un cliente inexistente.

## Solución enlazada

Después de tu intento consulta [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

## Reto adicional

Diseña, sin implementar todavía, la frontera que necesitarías para guardar clientes en disco sin modificar `createClient`.

## Resumen

Ya tienes una aplicación full-stack pequeña, tipada y probada, con una deuda intencional claramente visible: persistencia.

## Siguiente paso

Continúa con [Lección 05 — Proyectos y estados que no se contradicen](05-proyectos-y-estados-tipados.md).

## Referencias

- [Node.js HTTP](https://nodejs.org/api/http.html)
- [Fetch API — MDN](https://developer.mozilla.org/docs/Web/API/Fetch_API)

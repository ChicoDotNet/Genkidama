# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir

Demostrarás que puedes leer, modificar, probar y explicar FreelanceDesk sin seguir una receta de archivos o líneas. Esta lección integra el curso completo: TypeScript estricto, validación runtime, HTTP, persistencia durable, pruebas, diagnóstico y hardening.

## Antes de empezar

Completa la [Lección 16](16-hardening-http-y-checkpoint-04.md). Desde `app/` ejecuta:

```bash
npm install
npm run verify
npm start
```

Confirma que puedes crear clientes, proyectos y cotizaciones, consultar estado, provocar una entrada inválida sin mutar datos y reiniciar conservando el archivo JSON.

## El problema

Un freelancer ya usa FreelanceDesk y pide una evolución pequeña: necesita fechas objetivo en proyectos y una vista de proyectos vencidos. Al mismo tiempo existe un bug de integridad: hoy pueden crearse proyectos con el mismo `id` si dos entradas externas convergen sobre ese identificador. El cambio debe conservar los contratos existentes y no adelantar memoria cuando la persistencia falla.

## Concepto

Una evaluación profesional no mide memoria de sintaxis. Mide si puedes **leer → formular → probar → implementar → diagnosticar → verificar → explicar**.

Los tipos ayudan a expresar el diseño, pero los datos externos siguen entrando como información no confiable. La solución debe mantener claras las fronteras entre dominio, HTTP, persistencia y presentación.

## Demostración

[DEMO] Antes de editar, recorre `src/domain`, `src/server`, `src/client` y las pruebas. Explica dónde debería vivir una regla de fecha objetivo, dónde se valida JSON externo y qué componente debe decidir cuándo una mutación ya es visible.

No implementes todavía.

## Código real

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md). Resuelve las historias sobre la misma aplicación canónica.

Puedes consultar documentación oficial, el compilador, las pruebas y las herramientas del navegador. No abras la solución de referencia antes de completar un intento.

## Qué acaba de pasar

Ya no estás reproduciendo un tutorial: estás manteniendo una base existente con contratos y riesgos acumulados. El objetivo es cambiar lo mínimo necesario sin esconder errores.

## Errores comunes

- Confiar en `Project` como si un body HTTP ya fuera un `Project` válido.
- Guardar `Date` directamente y descubrir después que JSON no preserva ese objeto.
- Marcar memoria como actualizada antes de que `save()` termine.
- Corregir el duplicado sólo en la UI.
- Usar `any`, assertions amplias o `!` para silenciar TypeScript.
- Optimizar consultas sin medir primero.
- Añadir logging de nombres, correos o payloads al diagnosticar.

## Buenas prácticas

Mantén `strict`, valida en fronteras, modela fechas serializables explícitamente, conserva snapshots candidatos para mutaciones durables y agrega pruebas que fallen por el comportamiento que estás cambiando.

## Tu turno

[PAUSA PARA EJERCICIO] Completa las historias A–E de la evaluación. Después prepara una explicación de cinco minutos sobre:

- el modelo de tipos;
- validación runtime;
- consistencia entre memoria y persistencia;
- contratos HTTP;
- un tradeoff que decidiste no resolver todavía.

## Cómo comprobar

Como mínimo:

```bash
npm run verify
npm start
```

Además comprueba manualmente una fecha válida, una fecha inválida, la consulta de vencidos, un ID duplicado, un fallo de persistencia y una recarga del proceso.

Autoevalúate con [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md).

## Solución enlazada

Sólo después de tu intento compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). La referencia muestra una dirección posible; tu implementación no necesita coincidir línea por línea.

## Reto adicional

Explica qué cambiaría si varios procesos pudieran modificar el mismo archivo o si el store migrara a una base de datos multiusuario. No implementes concurrencia distribuida: identifica el contrato que dejaría de ser suficiente.

## Cómo hablar de este proyecto en una entrevista

Cuenta primero el problema de producto y después las decisiones técnicas. Explica que TypeScript protege contratos estáticos, pero que HTTP/JSON siguen necesitando validación runtime; que el dominio no conoce Node, DOM ni filesystem; que `AppStateStore` permite cambiar persistencia; y que una mutación sólo se vuelve visible después de persistir el snapshot candidato.

Preguntas probables:

- ¿Qué diferencia hay entre un tipo TypeScript y validar un body HTTP?
- ¿Por qué `unknown` es preferible a `any` en una frontera externa?
- ¿Cómo evitas memoria adelantada cuando falla el filesystem?
- ¿Dónde pondrías una regla de negocio y dónde un status HTTP?
- ¿Qué limitaciones conserva la persistencia JSON?
- ¿Qué mide el diagnóstico y qué deliberadamente no registra?
- ¿Cuándo introducirías una base de datos o framework frontend?

## Resumen

Completar el curso significa poder modificar una aplicación full-stack TypeScript existente con pruebas y explicar tus decisiones. La evaluación produce evidencia práctica de competencia inicial; no garantiza contratación.

## Siguiente paso

Repite las áreas débiles de la rúbrica, conserva FreelanceDesk como evidencia y construye una variante propia antes de añadir complejidad de framework por inercia.

## Referencias

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [TypeScript narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
- [Node.js HTTP](https://nodejs.org/api/http.html)
- [Node.js File system](https://nodejs.org/api/fs.html)
- [Date and time formats — MDN](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)

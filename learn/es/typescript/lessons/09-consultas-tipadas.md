# Lección 09 — Consultas tipadas sin contaminar el dominio

## Qué vas a conseguir

Agregarás filtros combinables por cliente y estado para proyectos y cotizaciones sin meter reglas HTTP dentro del dominio.

## Antes de empezar

Completa la [Lección 08](08-json-confiable-y-checkpoint.md).

## El problema

Listar todo funciona al principio. En cuanto FreelanceDesk acumula datos necesitas preguntas como “proyectos activos” o “cotizaciones enviadas de este cliente”. Construir esas condiciones directamente dentro de cada ruta vuelve difícil probarlas y reutilizarlas.

## Concepto

Un filtro es una transformación pura: recibe una colección y criterios tipados, devuelve una vista nueva y no muta el origen. Los query strings siguen siendo texto externo, por lo que `status=paused` o `status=expired` deben validarse antes de llegar a la función de consulta.

## Demostración

[EN PANTALLA] Revisa `ProjectQuery`, `QuoteQuery`, `queryProjects` y `queryQuotes`.

[EJECUTAR]

```bash
npm test
```

Las pruebas combinan estado y cliente y comprueban que un valor externo desconocido produce 400.

## Código real

`GET /api/projects?status=active` convierte primero el texto con `parseProjectStatus`. `GET /api/quotes?clientId=...&status=sent` hace lo mismo con `parseQuoteStatus`. El dominio no sabe que los criterios llegaron por URL.

## Qué acaba de pasar

FreelanceDesk ahora tiene consultas útiles sin duplicar lógica de filtrado en HTTP, navegador o persistencia.

## Errores comunes

- Confiar en `as ProjectStatus` para un query string.
- Mutar el array con `splice` o `sort` durante una consulta.
- Hacer que una función de dominio reciba `URLSearchParams`.
- Convertir un filtro inválido en “sin resultados” y ocultar el error.

## Buenas prácticas

Valida en la frontera, conserva criterios pequeños y explícitos, y devuelve colecciones derivadas. Un query no debería cambiar el estado del sistema.

## Tu turno

Agrega un filtro opcional por `clientId` a proyectos y cotizaciones si no lo implementaste durante la demostración. Protege al menos una combinación con una prueba.

## Cómo comprobar

```bash
npm run verify
npm start
```

Prueba `GET /api/projects?status=active` y luego un estado inválido.

## Solución enlazada

Compara con el código canónico sólo después de tu intento.

## Reto adicional

Diseña, sin implementar todavía, cómo expresarías ordenamiento y paginación sin convertir `ProjectQuery` en un objeto indefinido de opciones.

## Resumen

Los filtros son reglas de lectura. TypeScript ayuda a hacer explícitos sus criterios, pero la frontera HTTP debe seguir validando runtime.

## Siguiente paso

Continúa con la [Lección 10](10-ciclo-comercial-de-cotizaciones.md).

## Referencias

- [TypeScript — Narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
- [URLSearchParams — MDN](https://developer.mozilla.org/docs/Web/API/URLSearchParams)

# Lección 14 — Tooling y gate profesional

## Qué vas a conseguir

Usarás un único gate reproducible para verificar tipos, build y pruebas antes de publicar un cambio.

## Antes de empezar

Completa la [Lección 13](13-errores-http-y-fronteras-explicitas.md).

## El problema

Ejecutar sólo la prueba que acabas de escribir puede dejar roto otro contrato. Ejecutar comandos diferentes localmente y en CI también crea falsos verdes difíciles de reproducir.

## Concepto

FreelanceDesk mantiene tooling deliberadamente pequeño. `npm run verify` encadena `tsc --noEmit`, build y `node:test`; el workflow `Learn TypeScript` usa el mismo comando. El gate no sustituye criterio: concentra evidencia repetible.

## Demostración

[EJECUTAR]

```bash
npm run verify
```

Después inspecciona `package.json` y `.github/workflows/learn-typescript.yml`. No hay un segundo conjunto oculto de reglas para CI.

## Código real

El proyecto conserva `strict`, `exactOptionalPropertyTypes` y las demás opciones del `tsconfig.json`. Un error de tipos no se arregla silenciándolo con `any` o `!`; se corrige la frontera que perdió información.

## Qué acaba de pasar

La misma orden que usa una persona para verificar el proyecto es la evidencia principal del runner. Eso reduce diferencias entre “funciona en mi máquina” y lo que realmente acepta el repositorio.

## Errores comunes

- Añadir linters o bundlers sólo para aumentar el número de checks.
- Deshabilitar `strict` ante el primer error incómodo.
- Hacer que CI ejecute pasos que el README no permite reproducir.
- Confundir compilación exitosa con comportamiento correcto.

## Buenas prácticas

Mantén gates aburridos, rápidos y explícitos. Agrega una herramienta sólo cuando reduzca un riesgo real mejor que el compilador y las pruebas existentes.

## Tu turno

[PAUSA PARA EJERCICIO] Introduce temporalmente una propiedad opcional usada como obligatoria y observa qué gate la detecta. Revierte el cambio y vuelve a verde.

## Cómo comprobar

```bash
npm run check
npm test
npm run verify
```

Los tres deben ser coherentes; `verify` es el contrato agregado.

## Solución enlazada

No existe código adicional que copiar: la solución es poder explicar por qué cada script existe y cuál riesgo cubre.

## Reto adicional

Propón una herramienta adicional y justifica su costo de instalación, mantenimiento y tiempo de CI. Si no mejora un riesgo concreto, no la añadas.

## Resumen

Tooling profesional no significa tooling abundante. Significa que el camino para detectar regresiones es conocido y repetible.

## Siguiente paso

Continúa con la [Lección 15 — Diagnóstico y rendimiento con evidencia](15-diagnostico-y-rendimiento-con-evidencia.md).

## Referencias

- [TypeScript Compiler Options](https://www.typescriptlang.org/tsconfig/)
- [`node:test`](https://nodejs.org/api/test.html)
- [npm scripts](https://docs.npmjs.com/cli/using-npm/scripts)

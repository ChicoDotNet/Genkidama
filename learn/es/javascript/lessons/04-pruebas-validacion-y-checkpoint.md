# Lección 4 — Pruebas, validación y checkpoint 01

## Qué vas a conseguir
Ejecutarás pruebas con el runner nativo de Node.js, entenderás validación en fronteras y modificarás una regla sin receta línea por línea.

## Antes de empezar
Desde `app/` ejecuta `npm test` y abre `tests/board.test.js`.

## El problema
Un tablero puede verse bien mientras acepta títulos vacíos, mueve ids inexistentes o corrompe estado anterior. Necesitamos evidencia repetible.

## Concepto
`node:test` organiza pruebas y `node:assert/strict` expresa expectativas. Probamos normalización, inmutabilidad, movimiento, errores y persistencia.

## Demostración
[EJECUTAR] Cambia temporalmente el mínimo de título en `normalizeTitle`, observa el fallo y restaura la regla.

## Código real
`normalizeTitle` exige texto entre 3 y 80 caracteres tras normalizar espacios. `moveCard` valida columna e id. La UI captura errores y los presenta en un elemento con `role="alert"`.

## Qué acaba de pasar
La validación no depende sólo de atributos HTML: la regla vive en JavaScript y queda protegida si mañana existe otra interfaz.

## Errores comunes
- probar implementación interna en vez de comportamiento;
- escribir una prueba que nunca ejecuta la función;
- capturar errores y ocultarlos sin mensaje.

## Buenas prácticas
Una prueba debe fallar por una razón clara y cada error esperado debe tener un contrato comprensible.

## Tu turno — Checkpoint 01
Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) sin abrir la solución.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
Ejecuta `npm run check`, `npm test` y `npm run smoke`.

## Solución
Cuando termines, compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

## Reto adicional
Añade una prueba que demuestre que dos tarjetas no pueden compartir id.

## Resumen
- pruebas rápidas permiten cambiar reglas con confianza;
- validación del dominio complementa al HTML;
- el checkpoint obliga a modificar comportamiento.

## Siguiente paso
En la [Lección 5 — Editar y eliminar](05-editar-y-eliminar.md) mantendrás al estado —no al DOM— como fuente de verdad mientras agregas operaciones reales.

## Referencias
- [`node:test`](https://nodejs.org/api/test.html)
- [`assert`](https://nodejs.org/api/assert.html)

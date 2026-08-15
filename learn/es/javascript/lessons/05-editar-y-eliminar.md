# Lección 5 — Editar y eliminar sin convertir el DOM en tu base de datos

## Qué vas a conseguir
Podrás editar y eliminar tarjetas conservando una sola fuente de verdad: el estado de JavaScript. Practicarás `map`, `filter`, copias con spread y manejo explícito de ids inexistentes.

## Antes de empezar
Completa el checkpoint 01 y ejecuta desde `app/`:

```bash
npm run check
npm test
```

Abre `src/board.js` y localiza `addCard` y `moveCard`.

## El problema
La aplicación ya crea y mueve tarjetas, pero una tarea real cambia: el título puede corregirse y una tarjeta puede dejar de ser necesaria. Un atajo tentador sería modificar directamente el `<li>` del navegador. Eso deja dos verdades: lo que muestra el DOM y lo que conserva `board`.

## Concepto
En Kanban Local el estado manda. Una operación de dominio recibe un tablero y devuelve otro:

```js
const edited = editCard(board, "card-1", "Preparar demo final");
const cleaned = deleteCard(edited, "card-1");
```

`map` es apropiado cuando conservas la colección pero reemplazas un elemento. `filter` es apropiado cuando produces una colección sin uno o más elementos.

## Demostración
[DEMO] En `editCard`, observa que primero reutilizamos `normalizeTitle`. La edición no crea una segunda regla de validación.

[EJECUTAR] Corre la prueba `edita una tarjeta sin mutar el tablero previo` y confirma que el tablero anterior conserva el título original.

## Código real
`editCard` busca por id, crea una copia de la tarjeta modificada y devuelve una copia del tablero. `deleteCard` verifica que el id exista y devuelve las tarjetas cuyo id es diferente.

La UI usa delegación de eventos: el `#board` recibe el clic y determina si el botón pide `move`, `edit` o `delete`. Después de una operación válida llama una sola ruta: persistir, renderizar y anunciar el resultado.

## Qué acaba de pasar
No tuvimos que enseñar al DOM a "recordar" nada. Después de cada cambio reconstruimos la vista desde `board`. Eso hace que filtros, persistencia y pruebas puedan compartir el mismo comportamiento.

## Errores comunes
- mutar `card.title` directamente y luego sorprenderte cuando una prueba comparte referencias;
- eliminar silenciosamente un id inexistente;
- validar el título sólo en el formulario HTML;
- duplicar la lógica de persistir/renderizar en cada botón.

## Buenas prácticas
- reutiliza funciones de validación en lugar de copiarlas;
- preserva operaciones de dominio deterministas y libres de DOM;
- trata el id como identidad y el título como dato editable;
- confirma acciones destructivas en la frontera de UI, no dentro del dominio.

## Tu turno
Agrega una prueba para demostrar que editar una tarjeta no altera su columna. Después agrega otra que compruebe que eliminar una tarjeta no modifica el tablero anterior.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

```bash
npm run check
npm test
npm run smoke
```

## Solución
Compara la forma de tus pruebas con `tests/board.test.js`. No necesitas inspeccionar el DOM para demostrar estas reglas.

## Reto adicional
¿Qué debería ocurrir si el usuario edita una tarjeta y escribe exactamente el mismo título? Decide si vale la pena optimizar ese no-op y explica por qué.

## Resumen
- `map` permite reemplazar un elemento sin mutar la colección original;
- `filter` expresa eliminación de forma declarativa;
- la UI dispara intención, pero el estado sigue siendo la fuente de verdad;
- una operación destructiva puede confirmarse sin contaminar la regla de dominio.

## Siguiente paso
En la [Lección 6 — Filtros y búsqueda](06-filtros-y-busqueda.md) separarás "qué existe" de "qué se está mostrando".

## Referencias
- [`Array.prototype.map`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
- [`Array.prototype.filter`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)

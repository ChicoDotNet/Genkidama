# Lección 6 — Filtros y búsqueda: derivar una vista sin destruir datos

## Qué vas a conseguir
Construirás búsqueda por texto y filtro por columna sin modificar el tablero persistido. Practicarás parámetros con valores por defecto, `filter`, composición de predicados y normalización para comparar texto.

## Antes de empezar
Debes poder crear, mover, editar y eliminar tarjetas. Ejecuta `npm test` para partir de una base verde.

## El problema
Cuando hay veinte tarjetas, recorrer las tres columnas deja de ser práctico. Queremos buscar "demo" o mostrar sólo `Terminado`. Pero filtrar no significa borrar: al quitar el filtro todas las tarjetas deben reaparecer.

## Concepto
Una **vista derivada** se calcula desde el estado original:

```js
const visible = filterCards(board, {
  query: "demo",
  column: "done",
});
```

La función no escribe en `board.cards`. Evalúa dos condiciones independientes:

1. la tarjeta pertenece a la columna solicitada, o el filtro es `all`;
2. el título contiene el texto buscado, ignorando mayúsculas/minúsculas.

## Demostración
[EN PANTALLA] Escribe `demo` en el buscador. Después cambia `Mostrar columna` a `Terminado`.

[EJECUTAR] Borra el texto y vuelve a `Todas`: el tablero completo reaparece porque nunca modificamos los datos.

## Código real
`filterCards` recibe un objeto de opciones con defaults:

```js
export function filterCards(board, { query = "", column = "all" } = {}) {
  // ...
}
```

Validamos que `query` sea texto y que la columna exista. Luego `render` obtiene `visibleCards` una sola vez y reparte esa vista por columna.

Los controles escuchan `input` y `change`. No guardamos la búsqueda en `localStorage`: es estado efímero de presentación, no información del tablero.

## Qué acaba de pasar
Separaste dos clases de estado:

- **estado de negocio:** tarjetas, títulos y columnas;
- **estado de vista:** texto buscado y filtro actual.

Esa distinción reduce bugs como "filtré y perdí mis tarjetas".

## Errores comunes
- reemplazar `board.cards` por el resultado filtrado;
- hacer búsquedas sensibles a mayúsculas sin intención;
- aceptar una columna arbitraria y devolver cero resultados silenciosamente;
- persistir cada carácter de búsqueda aunque no tenga valor de negocio.

## Buenas prácticas
Las funciones que derivan vistas deberían ser puras cuando sea razonable: misma entrada, misma salida, sin tocar DOM ni almacenamiento.

## Tu turno
Agrega dos tarjetas cuyos títulos compartan una palabra. Mueve una a `done` y escribe pruebas para:

1. buscar la palabra en todas las columnas;
2. combinar búsqueda + `done`;
3. demostrar que `board.cards.length` no cambia.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

```bash
npm test
```

Además prueba manualmente filtros alternados rápidamente. Ninguna tarjeta debe desaparecer del almacenamiento.

## Solución
La suite del curso contiene ejemplos de filtro por texto, por columna y por ambos criterios.

## Reto adicional
¿Cómo implementarías ordenamiento sin mutar `board.cards`? Investiga `toSorted()` y compara su intención con `sort()`.

## Resumen
- filtrar una vista no modifica la fuente de verdad;
- los predicados pequeños se componen;
- el estado efímero de UI no siempre merece persistencia;
- validar opciones evita fallos silenciosos.

## Siguiente paso
En la [Lección 7 — Accesibilidad y teclado](07-accesibilidad-y-teclado.md) harás que estas capacidades sean operables sin depender del mouse ni de arrastrar elementos.

## Referencias
- [`Array.prototype.filter`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)
- [`String.prototype.includes`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/String/includes)
- [`toLocaleLowerCase`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)

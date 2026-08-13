# Lección 1 — Tu primer tablero en ejecución

## Qué vas a conseguir
Vas a ejecutar una aplicación web real y reconocer HTML como estructura, CSS como presentación y JavaScript como comportamiento.

## Antes de empezar
Necesitas Node.js 24 LTS y un navegador moderno. Desde `app/`, ejecuta `npm start` y abre `http://127.0.0.1:4173`.

## El problema
Una lista plana no muestra flujo. Queremos distinguir trabajo pendiente, activo y terminado.

## Concepto
JavaScript se ejecuta directamente en el navegador. `index.html` carga `src/app.js` con `type="module"`, habilitando imports explícitos y evitando variables globales accidentales.

## Demostración
[DEMO] Abre el tablero. Agrega `Preparar propuesta`, muévela a `En curso` y luego a `Terminado`.

## Código real
Busca en `index.html` el formulario y las tres secciones `data-column`. Después abre `src/app.js`: ahí se escuchan eventos y se renderiza el estado.

## Qué acaba de pasar
El navegador recibió una interacción, JavaScript actualizó el estado y `render()` lo convirtió en DOM visible.

## Errores comunes
- abrir el archivo directamente y chocar con restricciones de origen;
- editar HTML para simular tarjetas en vez de modificar estado;
- poner todo dentro de un único `onclick`.

## Buenas prácticas
Ejecuta bajo HTTP local, usa módulos y conserva un solo lugar donde se renderiza cada columna.

## Tu turno
Cambia los títulos visibles de las columnas sin tocar la lógica. Agrega una tarjeta y confirma que aparece en `Por hacer`.

## Cómo comprobar
Recarga la página: la tarjeta debe permanecer porque el tablero ya se persiste localmente.

## Solución
Compara con `app/index.html` y conserva los selectores `data-column` estables.

## Reto adicional
Explica por qué `data-column="todo"` puede permanecer estable aunque el texto visible se traduzca.

## Resumen
- JavaScript aporta comportamiento;
- el DOM no debe ser la única fuente de estado;
- módulos y servidor local preparan una base profesional sin framework.

## Siguiente paso
En la [Lección 02](02-datos-objetos-arrays-y-render.md) modelaremos tarjetas con objetos y arrays.

## Referencias
- [JavaScript modules](https://developer.mozilla.org/docs/Web/JavaScript/Guide/Modules)

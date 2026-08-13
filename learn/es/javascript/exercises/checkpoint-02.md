# Checkpoint 02 — Limpiar trabajo terminado sin romper el tablero

## Objetivo
Agregar una acción **Limpiar terminadas** que quite todas las tarjetas de `done` y preserve las demás. Debes usar lo aprendido en las lecciones 5–8 sin convertir la UI en dueña de la regla.

## Requisitos

1. Crea en `board.js` una función pública `clearDone(board)` que:
   - no mute `board`;
   - elimine sólo tarjetas cuya columna sea `done`;
   - funcione si no hay tarjetas terminadas.
2. Agrega pruebas que demuestren:
   - una tarjeta `todo` sobrevive;
   - una tarjeta `done` desaparece;
   - el tablero anterior conserva ambas;
   - exportar e importar el resultado mantiene el mismo estado.
3. Agrega un botón `Limpiar terminadas` a la UI.
4. Antes de ejecutar una limpieza que realmente eliminará tarjetas, pide confirmación al usuario.
5. Después de limpiar:
   - persiste;
   - vuelve a renderizar respetando búsqueda/filtro actuales;
   - anuncia el resultado mediante la región `role="status"`.
6. El botón debe ser alcanzable y activable con teclado.

## Restricciones

- No manipules `<li>` directamente para "borrar" tarjetas.
- No leas `localStorage` desde `clearDone`.
- No desactives ni reescribas pruebas existentes para obtener verde.
- No agregues una dependencia para resolver este checkpoint.

## Cómo comprobar

```bash
npm run check
npm test
npm run smoke
```

Prueba además este escenario manual:

1. crea tres tarjetas;
2. deja una en cada columna;
3. busca una palabra que coincida con la tarjeta `todo`;
4. ejecuta `Limpiar terminadas`;
5. quita el filtro;
6. comprueba que sólo desapareció la tarjeta que estaba en `done`;
7. exporta el tablero e impórtalo nuevamente.

## Preguntas de reflexión

- ¿Por qué `clearDone` pertenece al dominio y la confirmación pertenece a la UI?
- ¿Qué diferencia hay entre filtrar visualmente `done` y eliminar realmente esas tarjetas?
- Si no había tarjetas terminadas, ¿conviene pedir confirmación? Defiende tu decisión.

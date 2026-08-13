# Lección 7 — Accesibilidad y teclado: una UI que no depende de arrastrar

## Qué vas a conseguir
Mejorarás la interacción del Kanban usando HTML semántico, etiquetas, botones operables por teclado, foco visible y regiones vivas para mensajes. Entenderás por qué accesibilidad es comportamiento verificable, no decoración.

## Antes de empezar
Abre Kanban Local y recórrelo una vez usando sólo `Tab`, `Shift+Tab`, `Enter` y `Space`.

## El problema
Un Kanban suele implementarse con drag-and-drop. Si ésa fuera la única forma de mover una tarjeta, una persona que usa teclado o tecnología asistiva podría quedar bloqueada. La aplicación también necesita comunicar "tarjeta editada" o un error sin depender únicamente de un cambio visual.

## Concepto
Partimos de controles nativos:

- `<button>` para acciones;
- `<label for>` para asociar texto con inputs/selects;
- `role="alert"` para errores que requieren atención;
- `role="status"` + `aria-live="polite"` para confirmaciones no urgentes;
- `:focus-visible` para que el foco sea perceptible.

Esto sigue el principio de **semántica primero**: no uses ARIA para reemplazar un control HTML que ya existe.

## Demostración
[DEMO] Sin tocar el mouse, crea una tarjeta y recorre sus botones. Puedes editar, eliminar o mover porque cada acción es un `<button>` real.

[EN PANTALLA] Inspecciona un botón generado. Además de su texto visible, recibe un `aria-label` contextual como `Mover Preparar demo a done`.

## Código real
La UI no implementa drag-and-drop en este punto. Los botones de movimiento son deliberados: enseñan eventos y mantienen una ruta accesible.

Después de una mutación, `persistAndRender` actualiza una región `role="status"`. Los errores permanecen en `role="alert"`. El CSS añade un outline explícito a `button`, `input` y `select` cuando el foco es visible.

## Qué acaba de pasar
La accesibilidad cambió decisiones de arquitectura de interacción:

- no existe una capacidad exclusiva del mouse;
- las etiquetas están programáticamente asociadas;
- el usuario recibe feedback de éxito y de error;
- el foco puede localizarse visualmente.

## Errores comunes
- quitar `outline` sin reemplazo;
- usar `<div onclick>` como botón;
- poner `aria-label` genéricos como "acción";
- hacer drag-and-drop sin alternativa;
- usar `role="alert"` para cada mensaje informativo y saturar a lectores de pantalla.

## Buenas prácticas
Prueba teclado temprano. Es mucho más barato mantener accesibilidad mientras construyes que reconstruir la interacción al final.

## Tu turno
Haz una auditoría manual mínima:

1. recorre todos los controles con `Tab`;
2. crea una tarjeta sin mouse;
3. mueve la tarjeta usando un botón;
4. activa búsqueda y filtro;
5. provoca un título inválido y comprueba que aparece un mensaje;
6. confirma que el foco siempre es visible.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
La suite unitaria protege dominio, no accesibilidad visual. Para esta lección combina `npm test` con la prueba manual de teclado. Más adelante podremos automatizar más superficie con tooling de navegador cuando el beneficio justifique la dependencia.

## Solución
Si una acción todavía exige apuntar o arrastrar, no has terminado la ruta de teclado.

## Reto adicional
Investiga el elemento nativo `<dialog>` y evalúa si sería mejor que `prompt/confirm` para edición y confirmación en una versión posterior. No lo agregues sólo por novedad.

## Resumen
- HTML nativo da comportamiento accesible antes de ARIA adicional;
- una capacidad no debe depender exclusivamente del mouse;
- `alert` y `status` comunican intenciones distintas;
- el foco visible es parte del contrato de interacción.

## Siguiente paso
En la [Lección 8 — Importar/exportar JSON y checkpoint 02](08-importar-exportar-json-y-checkpoint.md) cruzarás una nueva frontera: datos que vienen de un archivo externo.

## Referencias
- [WAI — Introduction to Web Accessibility](https://www.w3.org/WAI/fundamentals/accessibility-intro/)
- [MDN: ARIA live regions](https://developer.mozilla.org/docs/Web/Accessibility/ARIA/Guides/Live_regions)
- [MDN: `:focus-visible`](https://developer.mozilla.org/docs/Web/CSS/:focus-visible)

# Checkpoint 04 — Importación segura con vista previa

## Objetivo
Modificar Kanban Local para que importar un archivo sea una decisión explícita y reversible hasta el momento de confirmar.

No abras la solución antes de intentarlo.

## Situación actual
El flujo ya protege tamaño, JSON, versión e invariantes. Sin embargo, después de leer un archivo válido, `app.js` reemplaza inmediatamente `board` y lo persiste.

Eso es técnicamente válido, pero una acción destructiva de usuario merece una frontera adicional: **entender qué cambiará antes de aplicarlo**.

## Trabajo
Implementa un flujo con estas propiedades:

1. el archivo sigue pasando por `assertImportFileSize` e `importBoard`;
2. antes de modificar `board`, calcula una vista previa útil comparando estado actual y candidato;
3. muestra al usuario un resumen suficiente para decidir;
4. pide confirmación explícita;
5. si cancela, el tablero actual y su persistencia quedan intactos;
6. si acepta, recién entonces reemplazas el estado, limpias filtros y persistes;
7. la lógica de comparación debe ser una función pura y tener pruebas `node:test`;
8. ningún título del archivo debe inyectarse como HTML;
9. `npm run verify` debe seguir verde.

## Un resumen mínimo aceptable
Puedes mostrar, por ejemplo:

```text
Tablero actual: 6 tarjetas
Archivo: 9 tarjetas
Nuevas: 4
Ausentes respecto al actual: 1
¿Reemplazar el tablero?
```

No tienes que implementar merge. La operación continúa siendo **reemplazo completo**; la vista previa sólo hace visible su impacto.

## Restricciones
- no agregues una librería de diff;
- no dupliques `assertValidBoard`;
- no persistas el candidato antes de confirmar;
- no conviertas `commands.js` en una capa de DOM;
- no uses `innerHTML` para construir la vista previa;
- si agregas un módulo importado por `app.js`, recuerda que el gate PWA exigirá incluirlo en `APP_SHELL` y versionar el cache.

## Pruebas mínimas
Para tu función pura cubre al menos:

- mismos ids → 0 nuevas y 0 ausentes;
- un id nuevo → 1 nueva;
- un id que desaparece → 1 ausente;
- la función no muta ninguno de los dos tableros.

La confirmación visual puede seguir siendo una comprobación manual en este checkpoint; no necesitas introducir automatización de navegador sólo para simular `window.confirm`.

## Evidencia de entrega
Entrega:

- código;
- pruebas nuevas;
- una nota breve indicando dónde vive la lógica pura y dónde vive la decisión de UI;
- salida verde de `npm run verify`;
- comprobación manual de aceptar/cancelar.

## Criterio de éxito
Un compañero debe poder responder leyendo tu cambio:

- qué se valida antes de mostrar la vista previa;
- qué estado puede cambiar antes de confirmar (idealmente ninguno);
- qué código conoce el DOM y qué código sólo compara datos;
- qué prueba evita que una futura refactorización vuelva destructiva la cancelación.

Cuando termines, compara con [`../solutions/importacion-segura-04.md`](../solutions/importacion-segura-04.md).

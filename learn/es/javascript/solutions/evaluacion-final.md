# Solución de referencia — Evaluación final JavaScript

> Abre esta referencia sólo después de completar un intento. No existe una única solución correcta.

## Dirección de diseño

Una solución razonable mantiene `priority` como dato de dominio de la tarjeta y limita sus valores a `normal` y `alta`. La normalización de datos antiguos puede ocurrir en la frontera que convierte estado persistido/importado al modelo vigente: ausencia de `priority` se interpreta como `normal`. La UI sólo captura/muestra la decisión; no debe convertirse en la autoridad de la regla.

El filtro de prioridad es estado de presentación. Puede combinarse con la búsqueda existente sin persistirlo: al recargar se recupera el tablero, no necesariamente la vista temporal del usuario.

## Historia A — Comportamientos mínimos

La implementación de referencia debe proteger con pruebas equivalentes a éstas:

```js
assert.equal(normalizePriority(undefined), "normal");
assert.equal(normalizePriority("alta"), "alta");
assert.throws(() => normalizePriority("urgente"));
```

Además prueba que una tarjeta legacy sin prioridad se carga como `normal`, que export/import conserva `alta` y que el filtrado no muta las tarjetas.

No copies esos nombres si tu arquitectura expresa la regla de otra forma.

## Historia B — Duplicados

La referencia **rechaza la importación completa** cuando dos tarjetas comparten `id`. Primero valida y construye un candidato en memoria; sólo después de que todo el documento sea válido reemplaza el estado persistido. Así una falla no deja una importación parcial.

Una comprobación típica usa un `Set` durante validación:

```js
const ids = new Set();
for (const card of candidate.cards) {
  if (ids.has(card.id)) {
    throw new Error(`ID de tarjeta duplicado: ${card.id}`);
  }
  ids.add(card.id);
}
```

La prueba de regresión debe verificar dos cosas: se informa el duplicado y el tablero previo permanece igual.

## Historia C — Regresión

Ejecuta:

```bash
npm run verify
npm start
```

No cambies `APP_SHELL`, cache version, CSP, límites de importación o contratos de persistencia salvo que tu implementación realmente lo requiera. Si agregas un módulo importado por la aplicación, el gate de integridad debe obligarte a mantener coherente el app shell.

## Historia D — Documentación

Una nota válida podría verificar en MDN que `Set` conserva valores únicos y usar esa propiedad para detección de IDs. Otra puede consultar IndexedDB para justificar por qué la actualización persistente sigue detrás del repositorio. Lo importante es enlazar la fuente oficial y explicar la decisión tomada.

## Historia E — Criterio esperado

La prioridad pertenece al modelo porque afecta significado y debe sobrevivir export/import. El filtro puede ser efímero porque representa una vista, aunque persistir preferencias sería una decisión de producto válida si se documenta. Más prioridades exigirían una migración compatible y una política explícita para valores desconocidos. Antes de optimizar miles de tarjetas se medirían tiempos de render/comandos y tamaño del estado con el diagnóstico existente. Para multiusuario, la primera frontera a reemplazar sería el repositorio local por un contrato de sincronización que trate identidad, versiones/conflictos y autorización; el dominio no debería depender directamente de HTTP.

## Defensa de entrevista

Una respuesta fuerte distingue **reglas**, **orquestación**, **I/O** y **presentación**. También reconoce límites: service worker no es sincronización, CSP no sustituye validación y una medición local no demuestra rendimiento en todos los dispositivos.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y puntúa tu solución por comportamiento y explicación, no por similitud de líneas con esta referencia.

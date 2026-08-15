# Evaluación final — Evoluciona Kanban Local sin receta

Trabaja sobre la aplicación canónica. No abras la solución hasta completar un intento.

## Historia A — Prioridad de tarjetas

Agrega prioridad `normal` / `alta` a las tarjetas.

- Una tarjeta nueva usa `normal` si no se especifica prioridad.
- La prioridad debe validarse en una frontera independiente del DOM.
- El usuario puede crear y editar la prioridad.
- La vista permite filtrar sólo tarjetas de prioridad alta sin perder búsqueda/estado existentes.
- Export/import conserva la prioridad.
- Datos antiguos sin el campo siguen cargando como `normal`.
- Agrega pruebas de regla, compatibilidad y comportamiento inválido.

No se prescribe el archivo, función ni estructura exacta.

## Historia B — Bug de importación

Hoy una importación puede contener dos tarjetas con el mismo `id`. Define e implementa una política segura y determinista para ese caso. Debe:

- rechazar o resolver explícitamente el duplicado; nunca aceptarlo de forma ambigua;
- dejar el tablero actual intacto si la importación completa no es válida;
- producir un mensaje útil para el usuario;
- tener una prueba de regresión.

Documenta en una frase por qué elegiste esa política.

## Historia C — Conserva contratos

Demuestra que siguen funcionando:

- `npm run verify`;
- creación, movimiento, edición y eliminación;
- búsqueda/filtros;
- IndexedDB/fallback previsto;
- export/import JSON;
- diagnóstico opt-in;
- app shell y assets PWA;
- límites y headers defensivos existentes.

No debilites una validación para conseguir verde.

## Historia D — Consulta documentación

Consulta al menos una fuente oficial de MDN o Node.js relacionada con una decisión de tu cambio. Entrega una nota breve con:

1. enlace;
2. qué verificaste;
3. qué decisión tomaste a partir de ello.

No se evalúa memorizar documentación: se evalúa usarla.

## Historia E — Diseño y operación

Escribe entre 180 y 300 palabras respondiendo:

- ¿Dónde debe vivir la regla de prioridad y por qué?
- ¿La selección actual del filtro debe persistirse? ¿Qué tradeoff implica?
- ¿Cómo migrarías datos si en una versión futura hubiera más prioridades?
- ¿Qué observarías antes de optimizar un tablero con miles de tarjetas?
- ¿Qué frontera cambiaría primero para soportar sincronización multiusuario?

## Entrega

Entrega código, pruebas, comandos ejecutados, resultado de la comprobación manual, nota de documentación y respuesta de diseño. Explica también un error que encontraste durante el trabajo y cómo lo diagnosticastes.

## Comprobación mínima

```bash
npm run verify
npm start
```

Después prueba en navegador: prioridad alta → filtro → exportar → importar → recargar; intenta un JSON con IDs duplicados y confirma que no destruye el tablero actual; comprueba online → offline → recarga.

Evalúate con [`rubrica-final.md`](rubrica-final.md).

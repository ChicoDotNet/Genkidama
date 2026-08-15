# Guía de autoría

## Antes de escribir

1. Lee `progress.yml`, `roadmap.md` y `decisions.md`.
2. Revisa PRs abiertos y CI antes de abrir trabajo nuevo.
3. Lee `course-spec.md` y `pedagogy.md`.
4. Selecciona el conjunto mínimo de skills ASBN aplicables.
5. Investiga brevemente el uso profesional actual del lenguaje.
6. Verifica versión, runtime/compiler y tooling en fuentes oficiales.
7. Confirma o refina la aplicación canónica y registra una decisión si cambia.

No se detiene el trabajo por una preferencia arquitectónica menor: se elige una opción reversible, se documenta y se continúa.

## Escribe para una persona principiante

Define un concepto la primera vez que lo usas.

Evita asumir que el lector conoce:

- terminal;
- compilador;
- paquete;
- dependencia;
- proceso;
- excepción;
- API;
- runtime;
- prueba automatizada.

Explica sólo lo necesario para el siguiente incremento visible.

## Mantén movimiento

Una buena lección produce algo observable:

- una salida nueva;
- una regla nueva;
- una pantalla;
- un archivo;
- una prueba;
- un reporte;
- un error mejor manejado.

Si varias páginas no cambian nada visible, revisa la secuencia.

## Código en el repositorio

No pegues como “fuente de verdad” un archivo grande en Markdown si el archivo real existe.

Enlaza:

`Ver implementación: ../app/...`

Evita números de línea que se rompen con cada edición.

## Ejemplos mínimos

Se permiten cuando reducen carga cognitiva.

Un ejemplo mínimo debe:

- enseñar un solo concepto;
- ser corto;
- volver rápidamente a la aplicación canónica;
- no convertirse en un segundo proyecto.

## Soluciones

Un ejercicio enlaza a la solución sólo después del intento:

`Ver solución después de intentarlo: ../solutions/...`

La solución explica por qué funciona y, cuando haya alternativas razonables, qué trade-off se eligió.

## Guion para instructor

Integra cues discretos:

- `[DEMO]`
- `[EN PANTALLA]`
- `[EJECUTAR]`
- `[PAUSA PARA EJERCICIO]`

Usa `> Nota para instructor:` sólo cuando ayude a anticipar una confusión, una pregunta o una demostración.

## SEO y descubribilidad por IA

Primero escribe para humanos.

Después comprueba que:

- el título describe lenguaje + resultado;
- las primeras secciones explican uso, requisitos y proyecto;
- los encabezados tienen significado fuera de contexto;
- FAQ responde preguntas reales;
- glosario define vocabulario del curso;
- no existe keyword stuffing;
- no se promete empleo.

## Empleabilidad

Antes de empezar un curso investiga sólo lo suficiente para decidir bien:

- usos actuales;
- tareas razonables para un junior;
- tooling habitual;
- proyecto demostrable.

Si el mercado es pequeño, dilo claramente.

No uses cifras de vacantes si no son necesarias.

## Dependencias

Añade una dependencia sólo si:

- elimina trabajo accidental considerable;
- representa tooling oficial o estándar;
- mejora de forma material la relevancia profesional;
- puede justificarse al alumno.

No añadas un framework porque “todo tutorial lo usa”.

## Windows y Linux

Cuando el ecosistema lo permita, verifica comandos equivalentes en:

- PowerShell;
- bash.

No agregues wrappers inútiles. Si `dotnet test` o `cargo test` ya expresa perfectamente la acción, un script que sólo llama ese comando no aporta valor.

## Revisión propia

Antes del commit:

- build;
- tests;
- formatter/linter si aplica;
- enlaces relativos;
- metadata;
- diff;
- secretos;
- instrucciones reproducibles.

Si algo no pudo ejecutarse, dilo en el PR.

## Pull Request

La descripción explica:

- incremento;
- aplicación/capacidad afectada;
- cómo probar;
- CI observado;
- decisiones;
- skills aplicadas;
- desviaciones;
- deuda o limitaciones;
- siguiente incremento.

Nunca hagas merge ni auto-merge como parte de la iniciativa autónoma.

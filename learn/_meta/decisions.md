# Decisiones de Genkidama Learn

Registro ligero de decisiones que una ejecución futura necesita conocer sin depender de memoria conversacional.

## GL-001 — `/learn` amplía el objetivo educativo

**Fecha:** 2026-08-12  
**Estado:** aceptada

Genkidama conserva su CLI/scaffolder y su aplicación comparativa de patrones. `learn/` amplía el frente educativo sin reemplazar ni rediseñar esos componentes.

Consecuencia: el trabajo nuevo se concentra en `learn/` y en workflows específicos cuando hagan falta.

## GL-002 — Español es la fuente canónica inicial

**Fecha:** 2026-08-12  
**Estado:** aceptada

Orden futuro de localización:

`es → en → zh-Hans → ja → fr → it → pt-BR → ru → de`

No se traducirá mientras la estructura y varios cursos en español no estén maduros.

## GL-003 — Una aplicación canónica por lenguaje

**Fecha:** 2026-08-12  
**Estado:** aceptada

La mayor parte de las lecciones incrementa una sola aplicación real. Ejemplos aislados son auxiliares.

Solidity conserva obligatoriamente `FreelanceEscrow` como concepto central.

## GL-004 — Carpeta de curso autosuficiente

**Fecha:** 2026-08-12  
**Estado:** aceptada

Una carpeta `learn/es/<slug>/` copiada fuera del monorepo debe conservar información y código suficientes para instalar, construir, probar y usar la aplicación.

No se crean dependencias ocultas hacia otros directorios de Genkidama.

## GL-005 — Pilotos antes de escala

**Fecha:** 2026-08-12  
**Estado:** aceptada

Secuencia inicial:

1. C#
2. Python
3. JavaScript
4. COBOL
5. Solidity

No se crean 45 esqueletos antes de demostrar que el formato funciona.

## GL-006 — El repositorio es la memoria operativa

**Fecha:** 2026-08-12  
**Estado:** aceptada

`progress.yml`, `roadmap.md` y este archivo permiten continuar desde una ejecución sin memoria previa.

El estado se actualiza por capacidad real, no para aparentar actividad.

## GL-007 — Git no es materia sustancial de los cursos

**Fecha:** 2026-08-12  
**Estado:** aceptada

Sólo se mencionan comandos inevitables para obtener o ejecutar el proyecto. Git tendrá un curso independiente.

## GL-008 — Skills ASBN como referencia preferente

**Fecha:** 2026-08-12  
**Estado:** aceptada

Los PR de implementación consultan preferentemente el catálogo versionado en `ChicoDotNet/ArquitectoDeSoluciones/skills/`.

Precedencia:

1. instrucciones explícitas del owner;
2. seguridad, privacidad, legal y regulación;
3. decisiones/contratos locales de Genkidama;
4. skill específica de stack/lenguaje;
5. skill ASBN general.

No se copian defaults específicos de otro producto.

## GL-009 — GitHub connector como ruta remota válida

**Fecha:** 2026-08-12  
**Estado:** aceptada

En entornos autónomos donde `gh`, DNS o terminal GitHub estén restringidos, esa condición no se redescubre en cada ejecución.

Si el conector GitHub puede realizar lectura, ramas, commits, refs, PRs y checks, se usa como ruta remota normal. La terminal se reserva para validaciones locales disponibles.

## GL-010 — CI de fundación separado

**Fecha:** 2026-08-12  
**Estado:** aceptada

La infraestructura de Learn tiene un workflow específico, acotado por paths, que valida catálogo, progreso, estructura y enlaces.

No modifica el workflow .NET existente del producto.

## GL-011 — PR principal por curso

**Fecha:** 2026-08-12  
**Estado:** aceptada

Preferencia: un PR activo por curso durante su construcción.

Se usan PRs stacked sólo cuando la dependencia mejora realmente continuidad/revisión.

Nunca merge ni auto-merge por parte de la iniciativa autónoma.

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

## GL-007 — Git no es materia sustancial de los cursos de lenguaje

**Fecha:** 2026-08-12  
**Estado:** aceptada, actualizada 2026-08-13

Sólo se mencionan dentro de cada curso de lenguaje los comandos inevitables para obtener o ejecutar el proyecto. Git se enseña en el curso transversal **ReleaseDesk**, ya completado, y no se duplica como materia sustancial en cada lenguaje.

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

## GL-012 — CI ejecutable aislado por curso/lenguaje

**Fecha:** 2026-08-12  
**Estado:** aceptada

Cada curso DEBE disponer de un gate de CI ejecutable independiente y acotado por paths del propio curso.

Un cambio localizado en `learn/es/vba/**`, por ejemplo, NO DEBE compilar ni ejecutar las pruebas de los otros 44 lenguajes. Debe ejecutar únicamente:

1. la validación común ligera de Genkidama Learn; y
2. el build/test/lint/smoke específico de VBA que técnicamente corresponda.

La misma regla aplica a todos los cursos.

La implementación preferida es un workflow por curso, por ejemplo `.github/workflows/learn-csharp.yml`, con filtros `paths` sobre `learn/es/csharp/**` y sobre su propio workflow. Puede sustituirse por un dispatcher/matriz dinámica sólo si preserva exactamente el mismo aislamiento observable.

Cambios puramente operativos en `progress.yml`, `roadmap.md`, `decisions.md`, catálogo o documentación común NO DEBEN provocar por sí mismos una fan-out de builds de los 45 toolchains.

Si en el futuro se introduce infraestructura ejecutable realmente compartida entre cursos, su cambio puede requerir una revalidación más amplia, pero esa expansión debe ser explícita y justificada; nunca accidental.

## GL-013 — Latest stable/LTS y cero deuda de deprecaciones silenciosa

**Fecha:** 2026-08-12  
**Estado:** aceptada

Genkidama Learn adopta como filosofía global usar tooling, runtimes, compiladores y GitHub Actions en versiones **estables y soportadas**.

Reglas:

1. Preferir la versión estable soportada más reciente cuando el ecosistema no distingue una línea de soporte prolongado.
2. Cuando exista una línea LTS adecuada para material educativo y producción, preferir la LTS activa más reciente frente a previews, RCs o versiones de soporte corto sin una ventaja explícita.
3. No adoptar previews/RC/nightly por novedad. Sólo se permiten por necesidad demostrable y documentada.
4. Una advertencia de CI sobre runtime, action, SDK, compiler, package manager o dependencia deprecada se considera trabajo de mantenimiento accionable; debe corregirse en el mismo frente o en el siguiente incremento razonable.
5. No ocultar deprecaciones con flags o variables de compatibilidad insegura como solución permanente. Un escape temporal sólo se acepta ante una emergencia concreta, con razón y plan de retiro documentados.
6. Antes de subir el major de una action o toolchain se consulta su documentación/release oficial para detectar requisitos de runner, breaking changes o sintaxis nueva.
7. La modernización debe conservar reproducibilidad: `course.yml` registra la versión probada y fecha real de verificación, aunque el workflow pueda usar un canal estable/LTS cuando eso sea intencional.
8. Los warnings de CI forman parte de la señal de calidad. Un gate verde con advertencias de deprecación conocidas no se considera estado ideal si existe una actualización soportada y razonable.

Aplicación inicial de esta decisión: `actions/checkout@v7` y `actions/setup-dotnet@v6` reemplazan generaciones basadas en Node 20 donde correspondía, sin cambiar el objetivo .NET 10 LTS/C# 14 del curso.

## GL-014 — Reporte ASBN SCRUMban por interacción autónoma

**Fecha:** 2026-08-12  
**Estado:** aceptada localmente

Cada lane autónomo de Genkidama Learn termina su interacción con un reporte breve y comparable basado en ASBN SCRUMban:

1. **¿Cómo estás?** Estado operativo actual del frente: sano, con deuda, con riesgo o esperando una interacción externa.
2. **¿En qué avanzaste desde la última interacción?** Cambios concretos y dos estimaciones: porcentaje del incremento/curso actualmente construido y porcentaje global estimado de Genkidama Learn v1. Los porcentajes son aproximados y deben explicar su base cuando pueda inducir a error.
3. **¿En qué planeas avanzar para la próxima interacción?** El siguiente incremento coherente, preferentemente sobre el mismo curso/PR incompleto.
4. **¿Qué te bloquea?** No significa bloqueo absoluto. Aquí se reporta cualquier obstáculo parcial que una interacción humana pueda desbloquear: información faltante, decisión entre alternativas con consecuencias relevantes, permisos/credenciales, acción del mundo real, revisión requerida o dependencia externa. Si nada requiere interacción humana, responder explícitamente `Nada`.

El reporte no sustituye pruebas, CI, `progress.yml`, roadmap ni decisiones. Es una vista ejecutiva del estado y debe ser factual, concisa y útil para decidir si intervenir.

Esta adopción local permanece aunque el formato se formalice posteriormente dentro de `asbn-senior-tdd-developer` o en un skill ASBN independiente de reporting.

## GL-015 — Debt First: liquidar deuda temprana en la siguiente interacción

**Fecha:** 2026-08-12  
**Estado:** aceptada localmente

Cuando una interacción descubre deuda técnica **acotada, verificable y razonablemente corregible** dentro del frente activo, la siguiente interacción DEBE comenzar liquidándola al 100% antes de expandir funcionalidad nueva.

La corrección no termina en editar el síntoma. Cuando sea proporcionado, debe añadir una defensa que reduzca la probabilidad de recurrencia: prueba de regresión, validador, regla de autoría, automatización o contrato explícito.

Excepciones: deuda legacy extensa, dependencia externa, riesgo desproporcionado o una corrección que requiera una decisión arquitectónica/material del owner. En esos casos la deuda debe quedar explícita, con alcance medible, secuencia de pago y siguiente objetivo concreto; no se deja como “luego”.

El owner usa como **heurística operativa** que diferir deuda hasta el final puede consumir del orden de 30% del esfuerzo del proyecto y que pagarla inmediatamente busca llevar ese overhead evitable hacia cero. Genkidama Learn adopta esa dirección como criterio de ejecución, no como estadística universal ni garantía matemática.

Aplicación inicial: un enlace faltante entre lecciones no sólo se corrige; el validador común comprueba navegación secuencial para impedir que el mismo tipo de deuda reaparezca silenciosamente.

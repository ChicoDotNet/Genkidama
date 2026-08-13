# Genkidama Learn — cursos de programación de 0 a Junior Developer

Genkidama Learn es la biblioteca educativa de Genkidama para aprender un lenguaje **construyendo una aplicación real** hasta alcanzar un nivel razonable de Junior Developer / Entry Level.

No promete empleo. Su objetivo es dejar evidencia práctica suficiente para que una persona pueda comenzar a solicitar oportunidades junior o pequeños proyectos freelance con una base profesional defendible.

## Qué significa “0 → Junior”

Al terminar un curso deberías poder leer y escribir código idiomático sencillo, manejar errores/I/O/persistencia cuando apliquen, usar tooling y pruebas, modificar una base existente, consultar documentación oficial, explicar arquitectura y resolver una evaluación final sin receta.

Git no se enseña aquí como materia principal. Tendrá su propio curso.

## Cómo se aprende

Todos los cursos siguen:

> **Problema → concepto → ejemplo mínimo si hace falta → aplicación real → ejercicio → prueba → reflexión → siguiente incremento**

Cada lenguaje tiene una sola aplicación canónica principal que crece durante el curso.

## Idioma

El contenido fuente canónico es español (`es`). Las traducciones se preparan para `en`, `zh-Hans`, `ja`, `fr`, `it`, `pt-BR`, `ru`, `de`, pero no comenzarán hasta que varios cursos en español estén maduros.

## Cursos piloto

| Orden | Lenguaje | Aplicación canónica | Estado |
|---|---|---|---|
| 1 | [C#](es/csharp/) | API de inventario, pedidos y facturación | **Completo** |
| 2 | [Python](es/python/) | Conciliador de facturas CSV con validación, persistencia y reportes | **Completo** |
| 3 | [JavaScript](es/javascript/) | Kanban offline-first/PWA principalmente nativo | **Completo** |
| 4 | [COBOL](es/cobol/) | Procesador batch de nómina | **En progreso** |
| 5 | Solidity | `FreelanceEscrow`: depósito, entrega, liberación y reembolso | Planeado |

No se crean carpetas vacías para simular avance. Un curso aparece cuando existe contenido y código coherentes.

## Qué contiene un curso

Una carpeta de curso debe ser suficientemente autónoma para copiarse fuera del monorepo y seguir funcionando. La forma objetivo incluye `README.md`, `course.yml`, `lessons/`, `app/`, `exercises/`, `solutions/` y sólo los directorios adicionales que aporten contenido real.

## Reglas de calidad

Un curso no se considera completo hasta tener entre 13 y 22 lecciones, aplicación funcional, instalación/build/test/run documentados, ejercicios/soluciones, CI, evaluación final/rúbrica, entrevista, referencias oficiales, enlaces válidos, metadata y validación desde una instalación razonablemente limpia.

## Catálogo y estado

La memoria operativa vive en:

- [Catálogo de lenguajes](./_meta/catalog.yml)
- [Estado de avance](./_meta/progress.yml)
- [Roadmap](./_meta/roadmap.md)
- [Decisiones](./_meta/decisions.md)
- [Pedagogía](./_meta/pedagogy.md)
- [Especificación de curso](./_meta/course-spec.md)
- [Guía de autoría](./_meta/authoring-guide.md)

## Alcance v1

Genkidama Learn v1 termina cuando los 45 lenguajes actuales tienen un curso completo en español, aplicación canónica funcional y validación razonable.

Delphi, GNU Octave, SQL, CSS, MicroPython y Rockstar están registrados como expansión posterior. Rockstar se presenta como contenido pedagógico/esotérico, no como una ruta con demanda laboral significativa.

## Licencia

El contenido y código originales de Genkidama Learn se publican bajo la licencia MIT de Genkidama.

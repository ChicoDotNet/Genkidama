# Genkidama Learn — cursos de programación de 0 a Junior Developer

Genkidama Learn es la biblioteca educativa de Genkidama para aprender un lenguaje **construyendo una aplicación real** hasta alcanzar un nivel razonable de Junior Developer / Entry Level.

No promete empleo. Su objetivo es dejar evidencia práctica suficiente para que una persona pueda comenzar a solicitar oportunidades junior o pequeños proyectos freelance con una base profesional defendible.

## Qué significa “0 → Junior”

Al terminar un curso deberías poder:

- leer y escribir código idiomático sencillo sin seguir una receta paso a paso;
- usar tipos, estructuras de datos, flujo de control y abstracciones propias del lenguaje;
- manejar errores, I/O y persistencia cuando correspondan;
- compilar o ejecutar, administrar dependencias básicas y depurar problemas habituales;
- escribir y ejecutar pruebas;
- modificar una base de código existente;
- consultar documentación oficial;
- explicar la arquitectura de la aplicación construida;
- resolver una evaluación final sin instrucciones paso a paso.

Git no se enseña aquí como materia principal. Tendrá su propio curso.

## Cómo se aprende

Todos los cursos siguen este ciclo:

> **Problema → concepto → ejemplo mínimo si hace falta → aplicación real → ejercicio → prueba → reflexión → siguiente incremento**

Cada lenguaje tiene **una sola aplicación canónica principal** que crece durante el curso. Los ejercicios aislados sólo aparecen cuando aclaran un concepto mejor que la aplicación principal.

## Idioma

El contenido fuente canónico es español (`es`).

La estructura queda preparada para traducirse más adelante, en este orden:

1. Inglés (`en`)
2. Chino simplificado (`zh-Hans`)
3. Japonés (`ja`)
4. Francés (`fr`)
5. Italiano (`it`)
6. Portugués de Brasil (`pt-BR`)
7. Ruso (`ru`)
8. Alemán (`de`)

Las traducciones no comenzarán hasta que varios cursos en español estén maduros.

## Cursos piloto

La primera etapa estabiliza la experiencia con cinco ecosistemas deliberadamente distintos:

| Orden | Lenguaje | Aplicación canónica | Estado |
|---|---|---|---|
| 1 | C# | API de inventario, pedidos y facturación | Planeado |
| 2 | Python | Conciliador de facturas CSV con validación, persistencia y reportes | Planeado |
| 3 | JavaScript | Kanban offline-first/PWA principalmente nativo | Planeado |
| 4 | COBOL | Procesador batch de nómina | Planeado |
| 5 | Solidity | `FreelanceEscrow`: depósito, entrega, liberación y reembolso | Planeado |

No se crean carpetas vacías para simular avance. Un curso aparece cuando existe contenido y código coherentes.

## Qué contiene un curso

Una carpeta de curso debe ser suficientemente autónoma para copiarse fuera del monorepo y seguir funcionando:

```text
learn/es/<curso>/
├── README.md
├── course.yml
├── lessons/
├── app/
├── examples/
├── exercises/
├── solutions/
└── tools/
```

No todas las carpetas tienen que existir desde el primer commit del curso; sí deben existir cuando aporten contenido real.

## Reglas de calidad

Un curso no se considera completo hasta tener:

- entre 13 y 22 lecciones;
- aplicación canónica funcional;
- instalación, build, test y run documentados;
- ejercicios y soluciones;
- pruebas automatizadas cuando sean técnicamente razonables;
- CI proporcional al ecosistema;
- evaluación final y rúbrica;
- sección para explicar el proyecto en una entrevista;
- referencias oficiales;
- enlaces internos válidos;
- metadata actualizada;
- validación desde una instalación razonablemente limpia.

## Catálogo y estado

La memoria operativa vive en el propio repositorio:

- [Catálogo de lenguajes](./_meta/catalog.yml)
- [Estado de avance](./_meta/progress.yml)
- [Roadmap](./_meta/roadmap.md)
- [Decisiones](./_meta/decisions.md)

Las reglas comunes están en:

- [Pedagogía](./_meta/pedagogy.md)
- [Especificación de curso](./_meta/course-spec.md)
- [Guía de autoría](./_meta/authoring-guide.md)

## Alcance v1

Genkidama Learn v1 termina cuando los 45 lenguajes actualmente detectados en Genkidama tienen un curso completo en español, una aplicación canónica funcional y validación razonable.

Delphi, GNU Octave, SQL, CSS, MicroPython y Rockstar están registrados como expansión posterior. En tecnologías esotéricas como Rockstar, el valor se explicará como pedagógico y transferible, no como una supuesta demanda laboral.

## Licencia

El contenido y código originales de Genkidama Learn se publican bajo la licencia MIT de Genkidama.

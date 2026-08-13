# Especificación de curso

Esta especificación define el contrato mínimo de un curso Genkidama Learn.

Las palabras **DEBE**, **NO DEBE**, **DEBERÍA** y **PUEDE** expresan prioridad normativa.

## 1. Identidad

Cada curso DEBE:

- tener un slug estable;
- vivir bajo `learn/es/<slug>/` mientras español sea la fuente canónica;
- tener un título descriptivo orientado a intención de búsqueda real;
- declarar una única aplicación canónica;
- poder copiarse fuera del monorepo sin depender de código oculto en otros directorios.

Ejemplo de título:

`Curso de COBOL desde cero — Construye un procesador de nómina`

## 2. Estructura

Contrato objetivo:

```text
learn/es/<slug>/
├── README.md
├── course.yml
├── lessons/
├── app/
├── examples/
├── exercises/
├── solutions/
└── tools/
```

No se crean directorios vacíos sólo para cumplir la forma. Se incorporan cuando contienen material real.

## 3. Metadata `course.yml`

Campos mínimos:

```yaml
schema_version: 1
slug: python
language: Python
locale: es
source_locale: es
title: Curso de Python desde cero — ...
status: in_progress
canonical_app:
  name: ...
  summary: ...
audience: beginner
prerequisites: []
lesson_count: 17
toolchain:
  verified_on: 2026-08-12
  version_tested: ...
  runtime_or_compiler: ...
  package_or_build_tool: ...
  operating_systems_tested:
    - windows-11
    - linux
ci:
  supported: true
  workflow: ...
career_context:
  uses: []
  market_note: ...
official_references: []
```

Reglas:

- `verified_on` DEBE representar una verificación real.
- No se inventan estadísticas de mercado.
- `market_note` DEBE reconocer limitaciones cuando la demanda sea pequeña.
- `lesson_count` DEBE coincidir con los archivos de `lessons/` cuando el curso esté completo.
- Metadata de tooling DEBE provenir de fuentes oficiales actuales al crear o actualizar el curso.

## 4. README del curso

Debe responder pronto:

- ¿Qué es el lenguaje?
- ¿Para qué se utiliza?
- ¿Puedo aprenderlo desde cero?
- ¿Qué voy a construir?
- ¿Qué necesito instalar?
- ¿Cuánto cubre el curso?
- ¿Qué sabré hacer al terminar?
- ¿Qué tipo de trabajo utiliza estas habilidades?

Además incluye:

- objetivos;
- prerrequisitos;
- tabla de contenidos;
- aplicación final;
- instalación;
- build;
- test;
- run;
- preguntas frecuentes;
- glosario;
- referencias oficiales;
- siguiente paso;
- sección de entrevista.

## 5. Lecciones

- DEBE haber entre 13 y 22 al completar el curso.
- El objetivo típico es 15–18.
- La lección 1 DEBE ejecutar algo.
- En las lecciones 2–4 la aplicación DEBE empezar a parecer real.
- La mayor parte del curso DEBE avanzar sobre la aplicación canónica.
- Ejemplos aislados sólo se usan cuando aclaran mejor un concepto.

## 6. Código

El código nuevo DEBE ser original y compatible con MIT.

DEBE priorizar:

1. estándar del lenguaje;
2. runtime/plataforma;
3. tooling oficial;
4. dependencias pequeñas justificadas.

Un framework sólo se vuelve columna vertebral cuando representa una ventaja laboral importante y el curso sigue enseñando el lenguaje, no únicamente el framework.

## 7. Errores y calidad

Desde el principio se enseñan:

- nombres claros;
- validaciones;
- errores explícitos;
- separación de responsabilidades;
- consistencia;
- seguridad básica cuando aplique;
- pruebas;
- documentación;
- idioms.

No se enseña código deliberadamente malo por ser más corto.

## 8. Ejercicios y soluciones

- Los ejercicios DEBEN obligar a escribir o modificar código.
- La solución NO DEBE aparecer inmediatamente en el ejercicio.
- Cuando sea posible se proporciona test o resultado esperado.
- Checkpoints cada 3–5 lecciones.
- La solución de referencia no sustituye el intento del estudiante.

## 9. Evaluación final

Debe extender la aplicación canónica y medir:

- lectura de código;
- funcionalidad nueva;
- bugfix;
- manejo de errores;
- prueba nueva;
- consulta de documentación;
- diseño de mejora.

Debe incluir rúbrica.

## 10. Tooling y plataforma

Objetivo general:

- Windows 11 + PowerShell + VS Code;
- Linux actual + bash + VS Code.

Excepciones técnicas se documentan al principio, por ejemplo Apple/Xcode, Office/VBA, Android o hardware MicroPython.

No se convierte el curso en una demostración comercial de IDE o nube.

## 11. CI

Cuando exista una vía razonable, cada curso DEBE tener un gate ejecutable propio que:

- instalar runtime/compiler;
- restaurar dependencias;
- compilar cuando aplique;
- ejecutar tests;
- demostrar que la aplicación principal puede construirse o ejecutarse;
- usar formatter/linter/static analysis sólo si es estándar y ligero.

El gate de un curso DEBE estar aislado por paths. Un cambio dentro de `learn/es/<slug>/**` NO DEBE disparar builds/tests de otros lenguajes.

La validación común ligera de metadata, estructura y enlaces PUEDE ejecutarse ante cualquier cambio en `learn/**`, pero no sustituye al CI específico de cada curso.

Cambios en `progress.yml`, `roadmap.md`, `decisions.md`, catálogo o documentación común NO DEBEN provocar accidentalmente una matriz de 45 toolchains.

Si aparece infraestructura ejecutable realmente compartida, la revalidación transversal debe ser explícita y justificada.

Una limitación real del runner se documenta; nunca se desactiva una prueba sólo para obtener verde.

## 12. Referencias y enlaces

Dentro del curso se prefieren enlaces relativos a archivos reales.

Referencias externas, en orden:

1. documentación oficial;
2. especificaciones oficiales;
3. repositorios oficiales;
4. proyectos open source reconocidos.

No se copian grandes fragmentos de terceros.

## 13. Skills de ingeniería

Los PR de implementación DEBEN seguir el gobierno publicado en `ChicoDotNet/ArquitectoDeSoluciones/skills/`.

El conjunto de skills debe ser el mínimo aplicable. Decisiones locales de Genkidama prevalecen sobre defaults genéricos.

La descripción del PR debe incluir:

```markdown
## Skills applied
- `skill-id`: por qué aplica

## Skill deviations
- Regla no aplicada: evidencia o decisión local
```

Documentación pura puede indicar `No implementation skill required`.

## 14. Definition of Done

No se marca `complete` hasta cumplir simultáneamente:

- 13–22 lecciones;
- aplicación funcional;
- instalación y ejecución;
- ejercicios y soluciones;
- pruebas cuando proceda;
- CI razonable;
- evaluación final;
- rúbrica;
- entrevista;
- referencias oficiales;
- README con navegación;
- enlaces internos válidos;
- metadata actualizada;
- prueba desde instalación razonablemente limpia.

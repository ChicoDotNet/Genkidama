# Pedagogía de Genkidama Learn

## Propósito

Genkidama Learn enseña a **resolver problemas con un lenguaje**, no a memorizar sintaxis. El resultado esperado es una persona principiante que puede construir, probar, explicar y extender una aplicación pequeña con hábitos profesionales razonables.

La meta no es afirmar que la persona “ya es profesional senior” ni prometer empleo. Es reducir la distancia entre “seguí un tutorial” y “puedo asumir una tarea junior con supervisión”.

## Ciclo obligatorio

Cada incremento pedagógico sigue:

> **Problema → concepto → ejemplo mínimo si hace falta → aplicación real → ejercicio → prueba → reflexión → siguiente incremento**

La teoría aparece cuando desbloquea una capacidad visible.

## Progresión

### Primer contacto

La primera lección debe permitir ejecutar algo. El estudiante conoce el runtime/compiler, el comando mínimo y la forma de observar un resultado.

### Primeras 2–4 lecciones

La aplicación canónica ya debe parecer un producto reconocible. No se consumen horas de sintaxis antes de construir.

### Núcleo

Se incorporan gradualmente, cuando el problema lo exige:

- sintaxis y tipos;
- estructuras de datos;
- control de flujo;
- funciones y módulos;
- abstracciones idiomáticas;
- manejo de errores;
- I/O;
- persistencia;
- pruebas;
- debugging;
- dependencias y build;
- arquitectura;
- idioms del ecosistema;
- seguridad, concurrencia y rendimiento sólo cuando correspondan.

### Cierre

El alumno extiende la misma aplicación sin receta paso a paso, corrige un bug, agrega una prueba y explica decisiones técnicas.

## Competencias 0 → Junior

Al completar un curso la persona debe poder:

1. Leer código idiomático razonablemente sencillo.
2. Escribir una aplicación pequeña sin copiar un tutorial.
3. Comprender tipos, estructuras de datos y flujo de control.
4. Separar responsabilidades con las abstracciones naturales del lenguaje.
5. Manejar errores sin ocultarlos.
6. Trabajar con archivos, entrada/salida o persistencia cuando el dominio lo requiera.
7. Depurar errores frecuentes.
8. Usar compiler/runtime e instrumentos básicos del ecosistema.
9. Instalar y administrar dependencias básicas.
10. Usar el build/package tool habitual.
11. Escribir y ejecutar pruebas.
12. Reconocer la estructura normal de un proyecto.
13. Identificar malas prácticas comunes.
14. Consultar documentación oficial.
15. Implementar una modificación en código existente.
16. Explicar la arquitectura del proyecto en una entrevista.
17. Resolver una evaluación final sin instrucciones paso a paso.

## Aplicación canónica

Cada curso tiene una sola aplicación principal:

- pequeña, pero genuinamente utilizable;
- local siempre que el dominio lo permita;
- sin APIs de pago obligatorias;
- con pocas dependencias;
- con errores explícitos;
- con pruebas;
- documentada;
- construible desde una copia limpia;
- suficientemente compleja para justificar separación de responsabilidades.

Los programas de juguete independientes son excepciones didácticas, no la columna vertebral.

## Forma de una lección

Entre 13 y 22 lecciones por curso; normalmente 15–18.

La forma recomendada es:

```markdown
# Lección NN — Título

## Qué vas a conseguir
## Antes de empezar
## El problema
## Concepto
## Demostración
## Código real
## Qué acaba de pasar
## Errores comunes
## Buenas prácticas
## Tu turno
## Cómo comprobar tu solución
## Solución
## Reto adicional
## Resumen
## Siguiente paso
## Referencias
```

No todos los encabezados son obligatorios siempre. La experiencia sí debe ser reconocible.

## Código y video en el mismo documento

Cada lección debe funcionar como:

1. material escrito;
2. guía práctica;
3. guion suficientemente detallado para impartirla.

Se permiten indicaciones breves:

- `[DEMO]`
- `[EN PANTALLA]`
- `[EJECUTAR]`
- `[PAUSA PARA EJERCICIO]`

Y, cuando realmente ayuda:

> Nota para instructor: ...

La página debe seguir siendo agradable para quien nunca verá el video.

## Ejercicios

El alumno escribe código. Un ejercicio de selección múltiple puede complementar, pero no sustituir, una tarea práctica.

Cuando sea razonable:

- el ejercicio vive en `exercises/`;
- la referencia vive en `solutions/`;
- hay test automatizado o resultado observable;
- la solución se enlaza después del intento, no se incrusta inmediatamente.

## Checkpoints

Cada 3–5 lecciones debe existir una comprobación breve que obligue a recuperar conocimiento anterior y modificar código.

Un checkpoint no necesita ser un examen formal. Sí necesita evidencia observable.

## Evaluación final

Sin instrucciones paso a paso. Debe medir al menos:

- comprensión de código existente;
- modificación funcional;
- corrección de un bug;
- manejo de errores;
- escritura de una prueba;
- consulta de documentación oficial;
- diseño de una pequeña mejora.

Debe incluir rúbrica que permita al estudiante distinguir entre:

- aún necesito práctica guiada;
- puedo intentar oportunidades junior con supervisión;
- puedo explicar y defender el proyecto con seguridad razonable.

## Entrevista

Cada curso termina con `Cómo hablar de este proyecto en una entrevista`.

Debe ayudar a explicar:

- problema;
- arquitectura;
- decisiones;
- trade-offs;
- pruebas;
- errores;
- una mejora futura.

No se proporcionan respuestas grandilocuentes ni se infla la experiencia del alumno.

## Git

Git no es una materia sustancial dentro de los cursos de lenguaje. El alumno debe aprenderlo en el [curso transversal de Git](../es/git/), que usa un laboratorio independiente y no exige conocimiento previo de programación.

Los cursos de lenguaje DEBEN recomendar y enlazar esa ruta cuando hablen de control de versiones, ramas, recuperación o colaboración. Pueden mostrar únicamente comandos inevitables para obtener o ejecutar material; no deben crear mini-cursos de Git duplicados dentro de cada lenguaje.

El curso de Git aplica el mismo ciclo problema → concepto → práctica → ejercicio → prueba, pero su aplicación canónica es un repositorio de trabajo y su evidencia principal son estados, historia, diffs e integraciones de Git.

## Regla de fondo

Un senior que revise el repositorio del estudiante debería poder pensar:

> “Es junior, pero aprendió bien.”

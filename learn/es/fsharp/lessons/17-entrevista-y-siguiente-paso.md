# Lección 17 — Cómo hablar de QuoteRules en una entrevista

## Qué vas a conseguir

Vas a cerrar el curso explicando con precisión qué construiste, qué decisiones tomaste, qué trade-offs aceptaste y qué mejorarías después.

## Cuenta el problema antes que la tecnología

Una explicación razonable empieza así, con tus propias palabras:

> QuoteRules es una CLI local que recibe partidas de una cotización, valida los datos, aplica reglas de precios y genera un reporte determinista. Separé parsing, dominio, pricing y persistencia para que los errores de una frontera no contaminaran las reglas de negocio.

Evita presentarte como experto senior por haber terminado el curso. Lo defendible es que puedes leer, probar y extender una aplicación pequeña con supervisión.

## Preguntas que deberías poder responder

### ¿Por qué F# para este problema?

Habla de funciones puras, records, uniones discriminadas, pattern matching y `Result` como herramientas para expresar reglas y estados con menos ambigüedad. Reconoce también el trade-off: F# tiene un mercado menor que C#, aunque comparte runtime y tooling .NET.

### ¿Cómo está organizada la aplicación?

Explica el flujo:

```text
entrada externa -> parsing/validación -> dominio -> pricing -> rendering -> persistencia
```

Aclara que `Pricing` no conoce archivos y que rendering puede probarse sin tocar el filesystem.

### ¿Qué error fue importante hacer explícito?

Elige uno real del proyecto: cantidad inválida, nivel de cliente desconocido, archivo de salida inválido o fallo operativo al guardar. Explica dónde se detecta y por qué no se oculta.

### ¿Qué pruebas aportan más valor?

Menciona contratos: reglas monetarias, parsing de datos inválidos, formato determinista y failure modes de persistencia. No presumas cobertura por sí sola; explica qué comportamiento protege cada test.

### ¿Qué mejorarías después?

Opciones razonables:

- un segundo formato de salida;
- política explícita de sobrescritura;
- más reglas configurables sin convertir strings en dominio;
- empaquetado como `dotnet tool` si existiera una necesidad real;
- observabilidad estructurada si la CLI creciera hacia un servicio.

Elige una y justifica la frontera donde viviría.

## Glosario final

- **Record:** tipo con campos nombrados, útil para datos del dominio.
- **Unión discriminada:** conjunto cerrado de casos posibles que permite modelar estados explícitos.
- **Pattern matching:** selección de comportamiento según la forma de un valor.
- **Función pura:** devuelve el mismo resultado para la misma entrada y no produce efectos observables externos.
- **`Result<'T,'TError>`:** representa éxito o error de forma explícita.
- **Pipeline (`|>`):** pasa el resultado de una expresión como argumento a la siguiente función.
- **Frontera:** punto donde el programa interactúa con texto, CLI, filesystem u otro sistema externo.
- **Determinista:** misma entrada y estado relevante producen la misma salida observable.

## FAQ

### ¿Puedo aprender F# desde cero con este curso?

Sí. La secuencia comienza ejecutando una cotización y aumenta complejidad sobre la misma aplicación. Conocer programación previa ayuda, pero no es un prerrequisito declarado.

### ¿Esto me convierte automáticamente en desarrollador F# profesional?

No. El objetivo es una base 0 → Junior: poder asumir cambios pequeños con supervisión, buscar documentación, escribir pruebas y explicar decisiones.

### ¿Necesito Windows?

No para el núcleo del curso. QuoteRules usa .NET y el gate del curso se valida en Linux y Windows.

### ¿Dónde aprendo Git?

En el [curso transversal de Git](../../git/). Este curso evita duplicarlo.

## Referencias oficiales para continuar

- [F# Guide](https://learn.microsoft.com/dotnet/fsharp/)
- [F# language reference](https://learn.microsoft.com/dotnet/fsharp/language-reference/)
- [F# core library reference](https://fsharp.github.io/fsharp-core-docs/)
- [.NET CLI overview](https://learn.microsoft.com/dotnet/core/tools/)
- [.NET support policy](https://dotnet.microsoft.com/platform/support/policy)

## Cierre

Si terminaste la evaluación, revisaste la rúbrica y puedes responder las preguntas anteriores sin leer una respuesta memorizada, ya tienes una evidencia concreta de aprendizaje: una aplicación pequeña que puedes construir, probar, depurar, extender y defender técnicamente.

[Anterior](16-rubrica-y-solucion.md) · [Volver al README](../README.md)

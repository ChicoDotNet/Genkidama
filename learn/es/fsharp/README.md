# Curso de F# desde cero — Construye un motor de precios y cotizaciones

F# es un lenguaje functional-first del ecosistema .NET. Este curso parte desde cero y construye **QuoteRules**, un motor local que valida partidas, aplica reglas de descuento y produce cotizaciones deterministas sin depender de servicios externos.

## Qué vas a construir

Una CLI pequeña pero real con dominio tipado, funciones puras para reglas de negocio, errores explícitos mediante `Result`, pruebas automatizadas, entrada desde archivo y persistencia local de reportes deterministas.

## Requisitos

- .NET 10 LTS SDK. La verificación usa SDK 10.0.111 con F# 10.
- VS Code o editor equivalente.
- PowerShell en Windows o bash en Linux.

Comprueba tu instalación con `dotnet --info`.

## Ejecutar

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj
```

Para cotizar desde un archivo con formato `descripcion|cantidad|precio`:

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj -- partner quote.txt
```

Para persistir además un reporte de texto:

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj -- partner quote.txt artifacts/quote.txt
```

## Probar

```bash
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj
```

## Lecciones

1. [Ejecuta tu primera cotización](lessons/01-primera-cotizacion.md)
2. [Modela el dominio con records y uniones discriminadas](lessons/02-modelar-dominio.md)
3. [Convierte reglas de precios en funciones puras](lessons/03-reglas-puras.md)
4. [Haz visibles los errores con Result](lessons/04-errores-result.md)
5. [Protege reglas con pruebas](lessons/05-pruebas.md)
6. [Procesa colecciones con pipelines](lessons/06-pipelines-colecciones.md)
7. [Compón funciones pequeñas](lessons/07-composicion-funcional.md)
8. [Lee entrada externa sin contaminar el dominio](lessons/08-entrada-externa.md)
9. [Checkpoint: cotiza desde datos externos](lessons/09-checkpoint-entrada.md)
10. [Usa tipos para proteger fronteras inválidas](lessons/10-tipos-para-fronteras.md)
11. [Produce reportes deterministas](lessons/11-reportes-deterministas.md)
12. [Persiste y diagnostica fallos operativos](lessons/12-persistencia-y-fallos.md)
13. [Checkpoint: cotización persistida](lessons/13-checkpoint-persistencia.md)
14. [Depura con evidencia y endurece el flujo](lessons/14-depuracion-con-evidencia.md)
15. [Evaluación final: extiende QuoteRules sin receta](lessons/15-evaluacion-final.md)
16. [Rúbrica y solución de referencia](lessons/16-rubrica-y-solucion.md)
17. [Cómo hablar de QuoteRules en una entrevista](lessons/17-entrevista-y-siguiente-paso.md)

La secuencia completa tiene **17 lecciones** sobre una sola aplicación canónica. Incluye checkpoints, depuración, evaluación final autónoma, rúbrica y solución de referencia.

## Ejercicio final y solución

- [Evaluación final independiente](exercises/evaluacion-final.md)
- [Solución de referencia, sólo después de intentarlo](solutions/evaluacion-final-referencia.md)

## Qué sabrás hacer al terminar

Leer y escribir F# idiomático de complejidad junior, modelar dominio, componer transformaciones, manejar errores, usar colecciones e I/O, probar reglas, depurar, construir con `dotnet` y explicar las decisiones del proyecto. El curso busca una base razonable para tareas junior con supervisión; no promete empleo.

## Preguntas frecuentes

### ¿Puedo aprender F# desde cero?

Sí. La primera lección ejecuta la aplicación y la complejidad crece sobre QuoteRules en vez de exigir teoría previa.

### ¿Necesito experiencia previa en .NET?

No. Ayuda conocer conceptos generales de programación, pero el curso no la declara como prerrequisito.

### ¿F# tiene el mismo mercado que C#?

No. F# tiene una comunidad profesional menor, aunque comparte runtime, bibliotecas y tooling .NET. El curso lo trata como una ruta para aprender modelado funcional y reglas de negocio, no como promesa de demanda laboral equivalente.

### ¿Necesito Windows?

No. El núcleo y sus pruebas se ejecutan con .NET en Linux y Windows.

### ¿Dónde aprendo Git?

En el [curso transversal de Git](../git/); aquí no duplicamos ese material.

## Glosario

- **Record:** tipo con campos nombrados para representar datos.
- **Unión discriminada:** conjunto cerrado de casos posibles.
- **Pattern matching:** selección de comportamiento según la forma de un valor.
- **Función pura:** no produce efectos externos y conserva el mismo resultado para la misma entrada.
- **`Result`:** tipo explícito para éxito o error.
- **Pipeline (`|>`):** encadena transformaciones pasando el resultado hacia la siguiente función.
- **Frontera:** punto donde el programa interactúa con texto, CLI, filesystem u otro sistema externo.
- **Determinista:** misma entrada y estado relevante producen la misma salida observable.

## Git

Para ramas, historial, recuperación y colaboración usa el [curso transversal de Git](../git/). Aquí sólo se mencionan comandos inevitables para trabajar con el proyecto.

## Contexto profesional

F# tiene un mercado más pequeño que C#, pero se ejecuta sobre .NET y se usa en dominios donde el modelado preciso, las transformaciones de datos y las reglas de negocio son valiosas. Muchas habilidades del curso son transferibles a otros lenguajes funcionales y al ecosistema .NET.

## Referencias oficiales

- [F# Guide](https://learn.microsoft.com/dotnet/fsharp/)
- [F# language reference](https://learn.microsoft.com/dotnet/fsharp/language-reference/)
- [F# Core reference](https://fsharp.github.io/fsharp-core-docs/)
- [What's new in F# 10](https://learn.microsoft.com/dotnet/fsharp/whats-new/fsharp-10)
- [.NET CLI overview](https://learn.microsoft.com/dotnet/core/tools/)
- [.NET support policy](https://dotnet.microsoft.com/platform/support/policy)

## Siguiente paso

Empieza por la [lección 1](lessons/01-primera-cotizacion.md). Si ya terminaste el curso, vuelve a la evaluación final sin mirar la solución y explica QuoteRules como lo harías ante una revisión técnica.

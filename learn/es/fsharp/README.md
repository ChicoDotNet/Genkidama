# Curso de F# desde cero — Construye un motor de precios y cotizaciones

F# es un lenguaje funcional-first del ecosistema .NET. Este curso parte desde cero y construye **QuoteRules**, un motor local que valida partidas, aplica reglas de descuento y produce cotizaciones deterministas sin depender de servicios externos.

## Qué vas a construir

Una CLI pequeña pero real con un dominio tipado, funciones puras para reglas de negocio, errores explícitos mediante `Result`, pruebas automatizadas, entrada desde archivo y persistencia local de reportes deterministas.

## Requisitos

- .NET 10 LTS SDK. La verificación inicial usa SDK 10.0.111 con F# 10.
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

Este incremento cubre 13 de las 17 lecciones previstas. Las cuatro restantes cerrarán hardening, evaluación final, rúbrica, entrevista y solución de referencia sobre la misma aplicación canónica.

## Qué sabrás hacer al terminar

Leer y escribir F# idiomático de complejidad junior, modelar dominio, componer transformaciones, manejar errores, usar colecciones e I/O, probar reglas, depurar, construir con `dotnet` y explicar las decisiones del proyecto. El curso busca una base razonable para tareas junior con supervisión; no promete empleo.

## Git

Para ramas, historial, recuperación y colaboración usa el [curso transversal de Git](../git/). Aquí sólo se mencionarán comandos inevitables para trabajar con el proyecto.

## Contexto profesional

F# tiene un mercado más pequeño que C#, pero se ejecuta sobre .NET y se usa en dominios donde el modelado preciso, las transformaciones de datos y las reglas de negocio son valiosas. Muchas habilidades del curso son transferibles a otros lenguajes funcionales y al ecosistema .NET.

## Referencias oficiales

- [F# Guide](https://learn.microsoft.com/dotnet/fsharp/)
- [What's new in F# 10](https://learn.microsoft.com/dotnet/fsharp/whats-new/fsharp-10)
- [.NET support policy](https://dotnet.microsoft.com/platform/support/policy)

## Próximo paso

Continúa por la lección 1 y ejecuta el programa antes de cambiarlo.

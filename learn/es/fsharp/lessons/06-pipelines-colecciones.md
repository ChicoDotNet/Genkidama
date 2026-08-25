# Lección 06 — Procesa colecciones con pipelines

## Qué vas a conseguir

Vas a transformar una secuencia de partidas externas en datos de dominio preservando el orden y deteniendo el proceso ante el primer error.

## El problema

QuoteRules ya calcula una cotización, pero sus partidas nacen como records dentro del código. Un programa real recibe colecciones de datos y necesita transformarlas sin perder claridad.

## Concepto

El operador `|>` pasa el resultado de una expresión como argumento a la siguiente función. En F# es una forma habitual de leer transformaciones de izquierda a derecha.

`Input.parseLines` recibe una `seq<string>`, usa `Seq.fold` para acumular un `Result<QuoteLine list,string>` y termina con `Result.map List.rev` para restaurar el orden original.

Ver implementación: [`../app/QuoteRules/Input.fs`](../app/QuoteRules/Input.fs)

## Qué acaba de pasar

La colección externa sigue siendo perezosa mientras es `seq<string>`, pero la frontera de dominio produce una lista concreta y validada. El error no se oculta: si una línea falla, el resultado completo es `Error`.

## Tu turno

Agrega una tercera partida a `sampleLines` en `Program.fs`, ejecútala y verifica que el subtotal cambie. Después cambia su cantidad a `0` y comprueba que la cotización falle explícitamente.

## Cómo comprobar tu solución

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj

dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj
```

## Buenas prácticas

Prefiere pipelines cuando aclaren el flujo de datos. No encadenes funciones sólo por estilo: si el pipeline oculta decisiones importantes, usa nombres intermedios.

## Resumen

Aprendiste a usar `|>`, `Seq.fold`, listas y `Result.map` sobre una colección real de la aplicación.

## Siguiente paso

En la próxima lección separarás parsing, validación y cálculo como funciones componibles.

## Referencias

- [F# collections](https://learn.microsoft.com/dotnet/fsharp/language-reference/fsharp-collection-types)
- [F# functions](https://learn.microsoft.com/dotnet/fsharp/language-reference/functions/)

[Anterior](05-pruebas.md) · [Siguiente](07-composicion-funcional.md)

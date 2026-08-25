# Lección 07 — Compón funciones pequeñas

## Qué vas a conseguir

Vas a entender por qué QuoteRules mantiene parsing, validación y cálculo como pasos pequeños que pueden probarse por separado y combinarse sin estado global.

## El problema

Una función que lee texto, interpreta números, valida reglas y calcula totales sería difícil de probar y de explicar. Necesitamos fronteras claras.

## Concepto

En F# una función puede producir exactamente el dato que otra función necesita. `Input.parseLine` convierte texto en `QuoteLine`; `Pricing.validateLine` protege invariantes; `Pricing.quote` consume una lista ya tipada y produce `Result<Quote,string>`.

La composición útil no significa escribir la menor cantidad de líneas. Significa mantener responsabilidades pequeñas y hacer explícitas las transiciones que pueden fallar.

Ver implementación: [`../app/QuoteRules/Input.fs`](../app/QuoteRules/Input.fs) y [`../app/QuoteRules/Pricing.fs`](../app/QuoteRules/Pricing.fs).

## Tu turno

Añade soporte para un alias `preferente` que produzca `Preferred`. Escribe primero una prueba en `QuoteRules.Tests`, después modifica `Input.parseTier` y vuelve a ejecutar la suite.

## Cómo comprobar tu solución

```bash
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj
```

La prueba nueva debe fallar antes del cambio y quedar verde después.

## Errores comunes

No conviertas todos los fallos en excepciones. Cuando un dato externo puede ser inválido de forma esperable, `Result` hace visible ese contrato y obliga al caller a decidir qué hacer.

## Resumen

Separaste transformación, validación y cálculo en funciones pequeñas, observables y testeables.

## Siguiente paso

Ahora la aplicación dejará de depender sólo de datos incrustados y aceptará entrada externa.

## Referencias

- [F# functions](https://learn.microsoft.com/dotnet/fsharp/language-reference/functions/)
- [F# error handling](https://learn.microsoft.com/dotnet/fsharp/language-reference/exception-handling/)

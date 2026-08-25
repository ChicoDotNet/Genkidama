# Lección 01 — Ejecuta tu primera cotización

## Qué vas a conseguir

Ejecutarás una aplicación F# real y aprenderás qué papel cumplen el SDK, el proyecto y `Program.fs`.

## El problema

Antes de estudiar sintaxis necesitas una señal observable: QuoteRules debe calcular una cotización y mostrar subtotal, descuento y total.

## Concepto

F# compila sobre .NET. El comando `dotnet run` restaura lo necesario, compila el proyecto y ejecuta su punto de entrada. El archivo `.fsproj` también define el orden de compilación de los archivos F#.

[DEMO]

Desde `learn/es/fsharp/` ejecuta:

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj
```

Debes observar una salida con `Subtotal`, `Descuento` y `Total`.

## Código real

Revisa [`../app/QuoteRules/Program.fs`](../app/QuoteRules/Program.fs). Todavía no necesitas entender cada símbolo. Identifica datos de entrada, llamada a `Pricing.quote` y las dos rutas posibles: `Error` y `Ok`.

## Errores comunes

- `dotnet` no reconocido: instala el SDK, no sólo el runtime.
- Ejecutar desde otra carpeta con una ruta incorrecta.
- Cambiar varios archivos antes de haber obtenido el baseline.

## Tu turno

Cambia únicamente la cantidad de `Consultoría`, vuelve a ejecutar y comprueba que el total cambia.

## Cómo comprobar tu solución

La aplicación debe seguir terminando con código de salida 0 y mostrar números coherentes con los datos modificados.

## Resumen

Ya ejecutaste una aplicación F# real. En la siguiente lección pondrás nombres y tipos al dominio que acabas de observar.

[Siguiente: modelar el dominio](02-modelar-dominio.md)

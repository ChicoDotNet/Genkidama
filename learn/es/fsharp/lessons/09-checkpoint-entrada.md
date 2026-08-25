# Lección 09 — Checkpoint: cotiza desde datos externos

## Qué vas a conseguir

Vas a demostrar que puedes conectar colecciones, funciones, parsing, `Result`, archivos y pruebas sin seguir una receta línea por línea.

## Reto

Partiendo del estado actual de QuoteRules:

1. crea un archivo con al menos tres partidas válidas;
2. ejecútalo para un cliente `partner`;
3. verifica manualmente subtotal, descuento y total;
4. crea un segundo archivo con una partida inválida;
5. confirma que la aplicación rechaza la entrada completa;
6. agrega una prueba automatizada para un failure mode que todavía no esté cubierto.

No cambies `Pricing.quote` para acomodar texto externo: el dominio debe seguir recibiendo `QuoteLine list`.

## Evidencia mínima

Debes poder mostrar:

```bash
dotnet build app/QuoteRules/QuoteRules.fsproj --configuration Release
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --configuration Release
dotnet run --project app/QuoteRules/QuoteRules.fsproj --configuration Release -- partner quote.txt
```

Y explicar con tus palabras:

- por qué `Input.parseLines` devuelve `Result`;
- por qué el orden se restaura con `List.rev`;
- por qué `Pricing.fs` no lee archivos;
- qué diferencia hay entre una `seq` y una `list` en este flujo;
- dónde agregarías un nuevo nivel de cliente.

## Autoevaluación

El checkpoint está logrado si puedes introducir un error, localizar la capa responsable, escribir una prueba que lo reproduzca y corregirlo sin mezclar I/O con reglas de negocio.

## Reto adicional

Acepta líneas vacías como separadores ignorables, pero sólo después de escribir una prueba que defina el comportamiento esperado. Decide explícitamente si una línea con espacios debe ignorarse o considerarse inválida.

## Resumen

QuoteRules ya no es un ejemplo con datos incrustados: puede transformar entrada externa en dominio tipado, rechazar errores esperables y conservar las reglas de negocio testeables.

## Siguiente paso

El próximo incremento introducirá persistencia/configuración local y profundizará en diseño de tipos e integración con el ecosistema .NET.

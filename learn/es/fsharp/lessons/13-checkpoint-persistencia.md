# Lección 13 — Checkpoint: cotización persistida

## Qué vas a conseguir

Vas a integrar tipos, parsing, reglas puras, render determinista, persistencia y manejo de errores sin una receta paso a paso.

## Reto

Construye una ejecución reproducible que:

1. lea al menos tres partidas desde archivo;
2. reciba el nivel de cliente por CLI;
3. calcule la cotización;
4. guarde el reporte en un subdirectorio que todavía no exista;
5. produzca código de salida distinto de cero ante una ruta inválida;
6. añada una prueba relevante para un failure mode de esta frontera.

Ejemplo de ejecución:

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj -- partner quote.txt artifacts/quote.txt
```

## Evidencia mínima

```bash
dotnet build app/QuoteRules/QuoteRules.fsproj --configuration Release
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --configuration Release
dotnet run --project app/QuoteRules/QuoteRules.fsproj --configuration Release -- partner quote.txt artifacts/quote.txt
```

Explica además:

- por qué `OutputFile` tiene constructor privado;
- por qué `render` no escribe archivos;
- qué errores puede prevenir el tipo y cuáles sólo puede detectar el filesystem;
- por qué el formato usa cultura invariante;
- dónde colocarías una segunda representación de salida sin contaminar `Pricing`.

## Autoevaluación

El checkpoint está logrado si puedes cambiar una regla de salida, protegerla con una prueba y demostrar que las reglas de precios permanecen intactas.

## Reto adicional

Define una política explícita para evitar sobrescribir un reporte existente. Decide si devolverías `Error`, generarías un nombre nuevo o exigirías un flag. Justifica el contrato antes de implementarlo.

## Siguiente paso

El siguiente incremento endurecerá observabilidad, depuración y composición de flujos antes de la evaluación final.

[Anterior](12-persistencia-y-fallos.md)

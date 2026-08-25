# Lección 08 — Lee entrada externa sin contaminar el dominio

## Qué vas a conseguir

Vas a ejecutar QuoteRules con datos provenientes de argumentos y de un archivo de texto, manteniendo el parsing fuera de las reglas de precios.

## El problema

Una app real no puede exigir recompilar para cambiar las partidas. Necesitamos una frontera de I/O que traduzca texto externo a tipos confiables.

## Formato de entrada

Cada línea usa:

```text
descripcion|cantidad|precio
```

Ejemplo:

```text
Consultoría|2|350
Implementación|1|600
```

El precio usa punto decimal porque `Input.parseLine` interpreta el dato con cultura invariante.

## Ejecuta con un archivo

Guarda las líneas anteriores en `quote.txt` y ejecuta:

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj -- preferred quote.txt
```

También puedes pasar sólo el nivel y conservar las partidas de ejemplo:

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj -- partner
```

## La frontera

`Program.fs` decide de dónde leer. `Input.fs` interpreta texto. `Pricing.fs` no conoce archivos, argumentos ni cultura de números.

Ver implementación: [`../app/QuoteRules/Program.fs`](../app/QuoteRules/Program.fs).

## Failure modes

Prueba deliberadamente:

- un archivo que no existe;
- un nivel `vip`;
- cantidad `dos`;
- precio `abc`;
- una línea sin los tres campos.

Cada caso debe terminar con mensaje explícito y código de salida distinto de cero, no con una cotización parcial silenciosa.

## Tu turno

Crea un archivo válido con tres partidas y otro con una cantidad inválida. Ejecuta ambos y explica por qué el segundo no llega a `Pricing.quote`.

## Resumen

Aprendiste a separar I/O, parsing y dominio, y a convertir datos externos poco confiables en valores tipados antes de calcular.

## Siguiente paso

Harás un checkpoint que combine colecciones, composición, parsing, errores y pruebas.

## Referencias

- [System.IO.File](https://learn.microsoft.com/dotnet/api/system.io.file)
- [F# command-line applications](https://learn.microsoft.com/dotnet/fsharp/get-started/get-started-command-line)

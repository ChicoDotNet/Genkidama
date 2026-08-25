# Lección 12 — Persistencia y fallos operativos

## Qué vas a conseguir

Vas a guardar el reporte en disco sin mezclar esa responsabilidad con las reglas de precios.

`Reporting.save` recibe un `OutputFile` ya validado, crea el directorio padre cuando hace falta y devuelve `Result<string,string>` con la ruta final o un error explícito.

## Frontera de efectos

El núcleo de precios sigue siendo puro. El filesystem queda concentrado en una función pequeña que puedes identificar, probar y reemplazar sin reescribir `Pricing.quote`.

La CLI acepta ahora una tercera posición opcional:

```bash
dotnet run --project app/QuoteRules/QuoteRules.fsproj -- partner quote.txt salida/cotizacion.txt
```

## Failure modes

Una ruta con extensión incorrecta falla antes del I/O. Un problema de permisos o filesystem falla durante `save`. Ambos llegan al usuario como error y código de salida distinto de cero; no se reporta éxito parcial.

## Ejercicio

Escribe una prueba que guarde en un subdirectorio temporal inexistente y demuestre dos contratos: se crea el directorio y el archivo contiene el total esperado. Limpia siempre los temporales en `finally`.

## Diagnóstico

Cuando investigues un fallo, conserva la entrada exacta, identifica si ocurrió en parsing, pricing, render o persistencia y agrega una prueba en la frontera responsable. No agregues `try/with` alrededor de todo el programa para ocultar el origen.

## Referencias oficiales

- [System.IO.File](https://learn.microsoft.com/dotnet/api/system.io.file)
- [System.IO.Directory](https://learn.microsoft.com/dotnet/api/system.io.directory)
- [F# Result](https://learn.microsoft.com/dotnet/fsharp/language-reference/results)

[Anterior](11-reportes-deterministas.md) · [Siguiente](13-checkpoint-persistencia.md)

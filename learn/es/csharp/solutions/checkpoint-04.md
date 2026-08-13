# Solución de referencia — Checkpoint 04

Una solución pequeña puede implementar middleware antes de mapear endpoints:

```csharp
app.Use(async (context, next) =>
{
    const string headerName = "X-Request-Id";
    var supplied = context.Request.Headers[headerName].FirstOrDefault();
    var requestId = !string.IsNullOrWhiteSpace(supplied) && supplied.Length <= 64
        ? supplied
        : Guid.NewGuid().ToString("N");

    context.Response.Headers[headerName] = requestId;

    using (app.Logger.BeginScope(new Dictionary<string, object?>
    {
        ["RequestId"] = requestId
    }))
    {
        await next(context);
    }
});
```

Y una prueba HTTP puede comprobar que una respuesta contiene el header incluso cuando el cliente no lo envía.

## Por qué esta solución es razonable

- la correlación pertenece a la frontera HTTP, no al dominio de pedidos;
- limita el tamaño del valor controlado por el cliente;
- no registra el body ni convierte el identificador en autenticación;
- usa un scope para que proveedores estructurados puedan adjuntar `RequestId` a los eventos de la petición.

No es la única solución correcta. También puedes usar `HttpContext.TraceIdentifier` si explicas claramente cómo lo expones y pruebas.

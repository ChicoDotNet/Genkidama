# Lección 14 — Documentación de API y contratos con OpenAPI

## Qué vas a conseguir

Vas a convertir las rutas de StockFlow en un contrato descubrible por herramientas sin mantener a mano un segundo documento que inevitablemente se desactualice.

## El problema

Saber que existe `/api/orders` porque leíste `Program.cs` no escala. Un consumidor necesita conocer rutas, operaciones, respuestas y esquemas sin inspeccionar el código fuente.

## Concepto

OpenAPI describe una API HTTP en un formato estándar independiente del lenguaje. ASP.NET Core 10 tiene soporte oficial para generar el documento a partir de endpoints Minimal API.

StockFlow usa `Microsoft.AspNetCore.OpenApi` y publica el documento **sólo en Development**.

## Demostración

[DEMO]

En `Program.cs` localiza:

```csharp
builder.Services.AddOpenApi();
```

Y después:

```csharp
if (app.Environment.IsDevelopment())
{
    app.MapOpenApi();
}
```

Inicia con entorno Development y abre:

```text
/openapi/v1.json
```

La suite también verifica que `/api/orders` aparezca en `paths`.

## Código real

Los endpoints agregan metadata pequeña pero útil:

```csharp
.WithName("CreateOrder")
.WithSummary("Crea un pedido, descuenta inventario y persiste el resultado")
.Produces<Order>(StatusCodes.Status201Created)
.ProducesProblem(StatusCodes.Status400BadRequest);
```

El contrato está junto al comportamiento que describe.

## Qué acaba de pasar

No agregamos Swagger UI ni otra dependencia visual. El documento JSON ya sirve para documentación, generación de clientes y validación automática; una UI puede agregarse después si existe una necesidad real.

## Errores comunes

- publicar documentación interna indiscriminadamente en producción;
- describir `200` cuando el código devuelve `201`;
- convertir el summary en una novela;
- confiar en OpenAPI como sustituto de probar el comportamiento real.

## Buenas prácticas

Nombra operaciones por intención, declara respuestas importantes y mantén la documentación junto al endpoint. Protege la exposición del documento según el entorno y sensibilidad del sistema.

## Tu turno

Agrega metadata OpenAPI completa a `GET /api/products/{sku}`: nombre, summary, respuesta 200 y ProblemDetails 404. Después verifica el JSON generado.

## Cómo comprobar

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj --filter OpenApi
```

## Reflexión

Si el código y una wiki discrepan, ¿cuál creerá un consumidor automático? ¿Cómo reduce ese riesgo un contrato generado desde metadata ejecutable?

## Resumen

- OpenAPI vuelve descubrible el contrato;
- ASP.NET Core genera OpenAPI 3.1 oficialmente;
- Development-only es una decisión de exposición, no una limitación del formato.

## Siguiente paso

Un contrato dice qué debería pasar. Los logs ayudan a explicar qué pasó cuando producción se comporta distinto.

## Referencias

- [OpenAPI en ASP.NET Core](https://learn.microsoft.com/aspnet/core/fundamentals/openapi/overview)
- [Generar documentos OpenAPI](https://learn.microsoft.com/aspnet/core/fundamentals/openapi/aspnetcore-openapi)

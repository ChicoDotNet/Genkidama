# Lección 13 — Pruebas de endpoints y regresiones HTTP

## Qué vas a conseguir

Vas a probar StockFlow desde la frontera HTTP sin levantar un proceso externo ni depender de `curl` manual. Al terminar tendrás pruebas que detectan regresiones en rutas, códigos de estado, JSON y persistencia observable.

## Antes de empezar

Ejecuta:

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Las pruebas de dominio y persistencia existentes deben seguir verdes.

## El problema

Hasta ahora probamos muy bien piezas internas, pero todavía podríamos romper `Program.cs`: cambiar una ruta, devolver 200 donde debía ser 400 o dejar de serializar un contrato. Los tests unitarios de `ProductCatalog` no lo detectarían.

## Concepto

Una prueba de integración ejecuta varias piezas juntas. En ASP.NET Core, `WebApplicationFactory<Program>` puede arrancar la aplicación dentro de un servidor de pruebas y entregar un `HttpClient` realista.

No reemplaza las pruebas unitarias. Las complementa: pocas pruebas HTTP protegen los contratos principales; muchas pruebas pequeñas siguen protegiendo reglas específicas.

## Demostración

[DEMO]

Abre `StockFlowApiFactory.cs`. La fábrica sustituye SQLite por `InMemoryOrderRepository` para que cada prueba tenga un entorno rápido y controlable.

Después revisa `EndpointTests.cs` y reconoce Arrange, Act y Assert aunque no estén comentados.

[EJECUTAR]

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj --filter EndpointTests
```

## Código real

La prueba de producto inválido envía JSON al endpoint y verifica dos cosas observables:

```csharp
Assert.AreEqual(HttpStatusCode.BadRequest, response.StatusCode);
Assert.AreEqual("Producto inválido", problem.Title);
```

Eso protege el contrato HTTP, no una implementación privada.

## Qué acaba de pasar

StockFlow ahora tiene pruebas en tres escalas:

- reglas de dominio;
- repositorio SQLite;
- frontera HTTP.

Elige la escala más pequeña que pueda demostrar el comportamiento que te importa.

## Errores comunes

- hacer todas las pruebas como integración y volver lenta la suite;
- compartir una base mutable entre tests sin controlar estado;
- afirmar sólo `200 OK` sin comprobar el dato importante;
- acoplar el test a detalles internos que el cliente HTTP nunca ve.

## Buenas prácticas

Mantén los escenarios HTTP enfocados en contratos relevantes: éxito, error representativo y una interacción completa. Usa repositorios controlables en pruebas cuando la persistencia real ya está cubierta en otra capa.

## Tu turno

Agrega una prueba HTTP para `GET /api/products/{sku}` que demuestre tanto el caso existente como un SKU inexistente. Comprueba el `404` y el título del `ProblemDetails`.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

Ejecuta sólo `EndpointTests` y luego la suite completa. Ambas deben quedar verdes.

## Reflexión

¿Qué bug puede detectar una prueba HTTP que una prueba de `ProductCatalog` nunca vería?

## Resumen

- `WebApplicationFactory` prueba la aplicación desde HTTP;
- integración y unit tests resuelven riesgos diferentes;
- un contrato merece una prueba por comportamiento, no por línea de código.

## Siguiente paso

En la [Lección 14](14-openapi-y-contratos.md) haremos visible el contrato para personas y herramientas mediante OpenAPI.

## Referencias

- [Pruebas de integración en ASP.NET Core](https://learn.microsoft.com/aspnet/core/test/integration-tests)

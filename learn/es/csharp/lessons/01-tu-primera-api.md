# Lección 01 — Tu primera API en ejecución

## Qué vas a conseguir

En esta lección vas a ejecutar StockFlow y comprobar dos cosas visibles: que el servidor responde y que ya puede devolver un producto. No necesitas entender todavía cada línea de C#.

Al terminar podrás explicar, en términos sencillos, qué hacen `dotnet run`, una ruta HTTP y el archivo `Program.cs`.

## Antes de empezar

Necesitas el SDK de .NET 10 y una terminal. Comprueba:

```bash
dotnet --version
```

Si el comando no existe, instala el SDK antes de continuar.

## El problema

Queremos construir un sistema de inventario, pedidos y facturación. Si empezáramos memorizando palabras reservadas no tendríamos ninguna señal de progreso. Por eso primero vamos a levantar el esqueleto mínimo de la aplicación.

## Demostración

[EN PANTALLA] Abre [`../app/src/StockFlow.Api/Program.cs`](../app/src/StockFlow.Api/Program.cs).

Observa tres zonas:

1. creamos la aplicación;
2. registramos una dependencia llamada `ProductCatalog`;
3. declaramos endpoints.

[EJECUTAR]

```bash
dotnet run --project app/src/StockFlow.Api/StockFlow.Api.csproj --urls http://localhost:5073
```

En otra terminal:

```bash
curl http://localhost:5073/health
```

Debes recibir un objeto JSON cuyo estado sea `ok`.

Después:

```bash
curl http://localhost:5073/api/products
```

Ahora aparece el producto inicial de demostración.

## Concepto: un programa ejecutable

El archivo [`StockFlow.Api.csproj`](../app/src/StockFlow.Api/StockFlow.Api.csproj) describe el proyecto. `Microsoft.NET.Sdk.Web` indica que construimos una aplicación web sobre .NET. `net10.0` es el framework objetivo.

`Program.cs` es el punto de entrada. C# moderno permite escribir código de nivel superior sin envolverlo manualmente en una clase `Program` con un método `Main`.

No memorices todavía `WebApplication.CreateBuilder`. Lo importante es reconocer la secuencia:

**configurar → construir → mapear capacidades → ejecutar**.

## Código real

El endpoint más pequeño es:

```csharp
app.MapGet("/health", () => Results.Ok(new { status = "ok" }));
```

Lee la línea de izquierda a derecha:

- `MapGet`: atiende una petición HTTP GET;
- `"/health"`: es la ruta;
- `() =>`: define una función pequeña;
- `Results.Ok(...)`: devuelve HTTP 200;
- `new { status = "ok" }`: crea el dato que se serializa a JSON.

## Qué acaba de pasar

Ya compilaste y ejecutaste C# real. También consumiste el resultado desde fuera del programa, que es más parecido al trabajo backend que imprimir veinte ejercicios aislados en consola.

## Errores comunes

### `dotnet` no se reconoce

El SDK no está instalado o la terminal todavía no ve su ruta. Cierra y abre la terminal después de instalar.

### El puerto está ocupado

Cambia `5073` por otro puerto disponible tanto al ejecutar como al hacer `curl`.

### Modifiqué `Program.cs` y no veo el cambio

Detén la aplicación y vuelve a ejecutar. Más adelante veremos recarga y debugging.

## Buenas prácticas

Desde el primer día:

- ejecuta la aplicación después de un cambio pequeño;
- lee los errores completos antes de cambiar código al azar;
- evita introducir librerías cuando el runtime ya resuelve el problema;
- nombra rutas y capacidades por el dominio, no por ocurrencias temporales.

## Tu turno

Crea temporalmente un endpoint `/hello` que devuelva tu nombre en JSON. Hazlo funcionar y después elimínalo: no pertenece al producto final.

[PAUSA PARA EJERCICIO]

## Cómo comprobar tu solución

```bash
curl http://localhost:5073/hello
```

Debes obtener HTTP 200 y JSON válido.

## Reflexión

¿Qué parte pertenece a C# y qué parte pertenece a ASP.NET Core? No necesitas una definición perfecta. Empieza a distinguir lenguaje, runtime y framework.

## Resumen

- `dotnet` compila y ejecuta el proyecto.
- `Program.cs` inicia la aplicación.
- un endpoint convierte una ruta HTTP en comportamiento;
- StockFlow ya es ejecutable desde la primera lección.

## Siguiente paso

En la [Lección 02](02-productos-y-tipos.md) dejaremos de tratar al producto como “JSON cualquiera” y aprenderemos a representarlo con tipos de C#.

## Referencias

- [C#](https://learn.microsoft.com/dotnet/csharp/)
- [ASP.NET Core](https://learn.microsoft.com/aspnet/core/)

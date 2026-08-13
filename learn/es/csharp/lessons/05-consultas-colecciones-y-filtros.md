# Lección 05 — Consultas, colecciones y filtros

## Qué vas a conseguir

StockFlow dejará de obligar al consumidor a traer todo el catálogo para encontrar un producto. Agregarás búsqueda por texto, filtro por existencia y consulta por SKU.

En el camino usarás `IEnumerable<T>`, colecciones, parámetros opcionales, `null` y expresiones lambda sobre un problema visible.

## Antes de empezar

Ejecuta las pruebas existentes:

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Luego levanta la API.

## El problema

Con diez productos, `GET /api/products` parece suficiente. Con diez mil, descargar todo para encontrar “mouse” es una mala interfaz.

Queremos poder preguntar:

```text
GET /api/products?search=mouse
GET /api/products?maxStock=5
GET /api/products?search=mouse&maxStock=5
GET /api/products/LAP-001
```

## Concepto: una colección también puede ser una consulta

Abre [`ProductCatalog.cs`](../app/src/StockFlow.Api/Products/ProductCatalog.cs).

`List<Product>` es la colección mutable que conserva el catálogo en memoria. Cuando empezamos una búsqueda usamos otra abstracción:

```csharp
IEnumerable<Product> query = _products;
```

No hemos creado una segunda lista. Estamos construyendo una secuencia de operaciones que luego materializamos con `ToArray()`.

## Demostración: filtrar sólo cuando hay criterio

```csharp
if (!string.IsNullOrWhiteSpace(text))
{
    var normalizedText = text.Trim();
    query = query.Where(product =>
        product.Sku.Contains(normalizedText, StringComparison.OrdinalIgnoreCase) ||
        product.Name.Contains(normalizedText, StringComparison.OrdinalIgnoreCase));
}
```

La expresión `product => ...` es una lambda: una función pequeña que recibe un producto y devuelve `true` o `false`.

No necesitas memorizar la sintaxis. Lee la intención: **conserva los productos cuyo SKU o nombre contienen el texto**.

## Parámetros opcionales y `null`

La firma:

```csharp
Search(string? text, int? maxStock)
```

usa `?` porque ambos criterios pueden faltar.

`null` no significa “cero” ni cadena vacía. Significa que no hay valor. El código decide qué comportamiento corresponde a esa ausencia.

## Endpoint con query string

En [`Program.cs`](../app/src/StockFlow.Api/Program.cs):

```csharp
app.MapGet("/api/products", (string? search, int? maxStock, ProductCatalog catalog) =>
    Results.Ok(catalog.Search(search, maxStock)));
```

ASP.NET Core enlaza los parámetros `search` y `maxStock` desde el query string. La regla de búsqueda sigue en `ProductCatalog`; HTTP sólo entrega datos y devuelve el resultado.

## Tu turno

Agrega dos productos y prueba estas consultas:

```bash
curl "http://localhost:5073/api/products?search=lap"
curl "http://localhost:5073/api/products?maxStock=5"
curl "http://localhost:5073/api/products/LAP-001"
```

Después consulta un SKU inexistente. Observa la diferencia entre `200 OK` con datos y `404 Not Found`.

## Cómo comprobar tu solución

Existe una prueba llamada `Search_WithTextAndStockFilter_ReturnsOnlyMatchingProducts`.

Ejecuta:

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

## Errores comunes

### Devolver `_products` directamente

Expondrías la colección mutable interna. Una referencia externa podría depender de detalles que el catálogo debe controlar.

### Convertir a lista después de cada `Where`

Materializar demasiado pronto crea trabajo y ruido innecesario. Encadena la consulta y materializa al final.

### Convertir todo a mayúsculas sin pensar en comparación

Para esta búsqueda usamos `StringComparison.OrdinalIgnoreCase`, que deja explícita la intención de comparar sin distinguir mayúsculas.

## Buenas prácticas

- conserva la mutabilidad dentro del componente que la gobierna;
- expresa criterios con nombres y comparaciones legibles;
- devuelve una instantánea cuando no quieres exponer estado interno;
- distingue “sin filtro” de “filtro con valor cero”.

## Reflexión

¿`Search` debería saber que el filtro llegó por HTTP? No. Podría ser llamado mañana desde una aplicación de consola o una prueba.

## Resumen

- una colección puede recorrerse y transformarse sin duplicarla en cada paso;
- las lambdas permiten expresar criterios pequeños;
- `null` representa ausencia de valor;
- la API gana una capacidad útil sin mezclar HTTP con la regla de consulta.

## Siguiente paso

En la próxima lección profundizarás en LINQ. No para aprender una lista de métodos, sino para producir información que el negocio pueda usar.

## Referencias

- [Expresiones lambda en C#](https://learn.microsoft.com/dotnet/csharp/language-reference/operators/lambda-expressions)
- [LINQ](https://learn.microsoft.com/dotnet/csharp/linq/)

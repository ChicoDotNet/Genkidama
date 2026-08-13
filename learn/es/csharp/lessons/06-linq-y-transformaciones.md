# Lección 06 — Funciones, LINQ y transformaciones

## Qué vas a conseguir

Vas a leer consultas LINQ como una secuencia de decisiones y a combinarlas para responder preguntas reales del inventario.

No vamos a convertir LINQ en un catálogo de métodos. La meta es que puedas reconocer `Where`, `Select`, `OrderBy`, `Any`, `FirstOrDefault`, `GroupBy` y `Sum` cuando aparezcan en código profesional.

## El problema

StockFlow ya filtra productos, pero pronto un pedido necesitará responder preguntas diferentes:

- ¿existe este SKU?;
- ¿hay suficiente stock?;
- si el mismo SKU aparece dos veces, ¿cuántas unidades pide en total?;
- ¿cuál es el total monetario de varias líneas?

Esas preguntas aparecen en el código que estamos por construir.

## Concepto: transformar una secuencia

Piensa en LINQ como tuberías pequeñas:

```csharp
var results = products
    .Where(product => product.Stock <= 5)
    .OrderBy(product => product.Name)
    .ToArray();
```

Cada paso tiene una responsabilidad visible.

`Where` conserva elementos. `OrderBy` cambia el orden. `ToArray` ejecuta y materializa el resultado.

## `FirstOrDefault`: encuentra uno o acepta que no exista

En `GetBySku`:

```csharp
return _products.FirstOrDefault(product =>
    string.Equals(product.Sku, normalizedSku, StringComparison.OrdinalIgnoreCase));
```

El resultado es `Product?` porque puede no existir coincidencia. Esa posibilidad aparece en el tipo y obliga al consumidor a decidir qué hacer.

## `Any`: pregunta sin traer el elemento

En la validación de productos usamos `Any` para saber si ya existe un SKU. Cuando sólo necesitas un sí/no, comunicar esa intención es mejor que buscar una lista completa.

## `GroupBy` y `Sum`: preparar un pedido

Mira esta parte de `TryReserve`:

```csharp
var normalizedRequests = requests
    .GroupBy(request => request.Sku.Trim(), StringComparer.OrdinalIgnoreCase)
    .Select(group => new StockRequest(
        group.Key.ToUpperInvariant(),
        group.Sum(item => item.Quantity)))
    .ToArray();
```

Si un cliente manda `LAP-001` dos veces, reservar línea por línea sería fácil de implementar mal. Primero agrupamos por SKU y sumamos cantidades.

Ese pequeño pipeline evita que dos representaciones distintas del mismo producto produzcan reglas distintas.

## Métodos frente a bloques gigantes

`Search`, `GetBySku`, `TryAdd` y `TryReserve` son operaciones distintas sobre el mismo catálogo. Separarlas permite:

- nombrar la intención;
- probar comportamientos de forma aislada;
- evitar un endpoint enorme con condiciones anidadas.

Una función útil no tiene que ser minúscula; debe tener una responsabilidad que puedas explicar.

## Tu turno

Sin modificar `ProductCatalog`, usa `Search` desde una pequeña sección temporal de una prueba para obtener productos con stock máximo 5 y calcula el valor del inventario encontrado:

```csharp
var inventoryValue = products.Sum(product => product.UnitPrice * product.Stock);
```

Después elimina el experimento o conviértelo en una prueba con una expectativa clara.

## Prueba que protege la transformación

`TryReserve_WhenAnyLineHasInsufficientStock_DoesNotChangeAnyProduct` comprueba algo más importante que la sintaxis: **si una reserva completa no puede cumplirse, no se debe descontar parcialmente el inventario**.

Ese comportamiento nos prepara para los pedidos.

## Errores comunes

### LINQ como acertijo

Una sola expresión de quince operaciones puede ser técnicamente correcta y difícil de mantener. Introduce variables con nombre cuando separan conceptos.

### `First()` cuando puede no existir

`First()` lanza una excepción si la secuencia está vacía. Si ausencia es un resultado normal, `FirstOrDefault()` y un tipo nullable suelen expresar mejor el dominio.

### Mutar dentro de `Select`

`Select` comunica transformación. Esconde menos sorpresas si no lo conviertes en un lugar de efectos secundarios.

## Buenas prácticas

- usa LINQ para expresar intención, no para demostrar habilidad;
- materializa cuando necesitas una instantánea estable;
- representa la ausencia de datos en el tipo;
- prueba la regla de negocio resultante, no la implementación exacta del pipeline.

## Reflexión

¿Por qué agrupamos las solicitudes antes de descontar stock? Intenta explicar el bug que aparecería si sólo verificáramos cada línea individual contra la existencia inicial.

## Resumen

- LINQ permite filtrar, buscar, agrupar, ordenar y agregar datos;
- los nombres de los métodos ayudan a leer intención;
- `Product?` hace visible que una búsqueda puede fallar;
- una transformación correcta puede proteger una regla de negocio real.

## Siguiente paso

Ya tenemos catálogo y reserva atómica. Ahora construiremos el primer pedido y veremos cómo varios objetos colaboran sin convertirse en una clase gigante.

## Referencias

- [Operaciones de consulta LINQ](https://learn.microsoft.com/dotnet/csharp/linq/standard-query-operators/)
- [Tipos de referencia nullable](https://learn.microsoft.com/dotnet/csharp/nullable-references)

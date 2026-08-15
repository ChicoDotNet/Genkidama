# Lección 02 — Productos, variables y tipos que representan negocio

## Qué vas a conseguir

Vas a entender por qué StockFlow usa tipos concretos para un producto y cómo C# evita que tratemos un precio, una existencia o un identificador como si fueran intercambiables.

## El problema

Un inventario necesita saber al menos:

- qué producto es;
- cómo se identifica comercialmente;
- cómo se llama;
- cuánto cuesta;
- cuántas unidades existen.

Podríamos meter todo en cadenas de texto. Eso sería fácil al principio y costoso después: `"cuatro"` no sirve para restar inventario y `"18,500"` puede interpretarse distinto según cultura.

## Concepto: tipos

Abre [`Product.cs`](../app/src/StockFlow.Api/Products/Product.cs).

```csharp
public sealed record Product(
    Guid Id,
    string Sku,
    string Name,
    decimal UnitPrice,
    int Stock);
```

Aquí aparecen varios tipos importantes:

- `Guid`: identificador único;
- `string`: texto;
- `decimal`: números decimales apropiados para importes monetarios sencillos;
- `int`: enteros.

El compilador usa estos tipos para detectar errores antes de ejecutar.

## ¿Qué es un `record`?

Un `record` es un tipo de C# especialmente cómodo para datos. Genera comportamiento útil de igualdad y una representación clara sin escribir mucho código ceremonial.

No significa que todos los objetos deban ser records. Aquí encaja porque `Product` representa datos de un producto que viajan por nuestra API.

## La petición de alta

Abre [`CreateProductRequest.cs`](../app/src/StockFlow.Api/Products/CreateProductRequest.cs).

Notarás que no contiene `Id`: el cliente no decide el identificador interno. StockFlow lo crea.

Esta separación parece pequeña, pero introduce una idea profesional:

**el dato que recibes no tiene por qué ser idéntico al dato que guardas o devuelves.**

## Demostración

[EJECUTAR] Inicia StockFlow y crea un producto:

```bash
curl -X POST http://localhost:5073/api/products \
  -H "Content-Type: application/json" \
  -d '{"sku":"mouse-01","name":"Mouse inalámbrico","unitPrice":450,"stock":8}'
```

La respuesta incluye un `id` generado por el servidor y un SKU normalizado.

## Variables y expresiones

En [`ProductCatalog.cs`](../app/src/StockFlow.Api/Products/ProductCatalog.cs) aparece:

```csharp
var normalizedSku = request.Sku.Trim().ToUpperInvariant();
```

`var` no significa “sin tipo”. El compilador infiere el tipo a partir de la expresión. En este caso sigue siendo `string`.

La expresión encadena dos operaciones:

1. `Trim()` quita espacios en extremos;
2. `ToUpperInvariant()` normaliza mayúsculas sin depender de la configuración regional.

## Colecciones: por ahora sólo lo necesario

`List<Product>` representa una colección ordenada y modificable de productos.

Cuando `GetAll()` devuelve:

```csharp
return [.. _products];
```

creamos una copia. No entregamos la lista interna para que otro código pueda modificarla accidentalmente.

Más adelante veremos colecciones y LINQ con calma.

## Errores comunes

### Usar `double` para dinero sin pensarlo

`double` es excelente para muchos cálculos científicos, pero los importes decimales de negocio suelen modelarse mejor con `decimal`.

### Hacer todos los campos `string`

Pierdes validación del compilador y terminas convirtiendo datos repetidamente.

### Exponer directamente la colección interna

Otro componente podría alterar el catálogo sin pasar por reglas de negocio.

## Buenas prácticas

- elige tipos que representen el significado del dato;
- diferencia comandos/peticiones de entidades o resultados cuando tengan responsabilidades distintas;
- normaliza valores en una frontera clara;
- no uses `var` cuando vuelva ambiguo el código; úsalo cuando el tipo resulte evidente.

## Tu turno

Agrega temporalmente una propiedad `Category` de tipo `string` a `Product` y `CreateProductRequest`. Sigue los errores del compilador hasta que la aplicación vuelva a construir.

Después responde: ¿en qué lugares te obligó C# a actualizar el código y por qué eso es útil?

## Cómo comprobar tu solución

El build debe terminar sin errores y el POST debe aceptar `category`.

Después revierte el cambio: la categoría será una decisión de dominio posterior, no la introduciremos sin una necesidad visible.

## Resumen

- un tipo restringe y comunica qué clase de dato esperamos;
- `record` es útil para modelos de datos concisos;
- `var` sigue siendo tipado estáticamente;
- separar petición y resultado evita mezclar responsabilidades.

## Siguiente paso

En la [Lección 03](03-validacion-y-errores.md) veremos por qué “el JSON tiene la forma correcta” no significa “el dato es válido”.

## Referencias

- [Tipos integrados de C#](https://learn.microsoft.com/dotnet/csharp/language-reference/builtin-types/built-in-types)
- [Records](https://learn.microsoft.com/dotnet/csharp/fundamentals/types/records)

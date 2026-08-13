# Lección 03 — Validación y errores que el usuario puede entender

## Qué vas a conseguir

Vas a introducir reglas visibles del negocio sin esconder fallos ni depender de excepciones para entradas previsiblemente inválidas.

## El problema

Este JSON es técnicamente válido:

```json
{
  "sku": "",
  "name": "",
  "unitPrice": -100,
  "stock": -3
}
```

Pero ningún inventario serio debería aceptarlo.

La sintaxis correcta no garantiza una operación válida.

## Concepto: una frontera de validación

Abre [`ProductCatalog.cs`](../app/src/StockFlow.Api/Products/ProductCatalog.cs) y localiza `Validate`.

La función devuelve `string?`:

- una cadena describe el problema;
- `null` significa que no encontró ese tipo de error.

Este uso introduce los tipos anulables de referencia. Con `<Nullable>enable</Nullable>`, el compilador nos obliga a ser explícitos cuando un valor puede ser `null`.

## Condiciones

Una regla:

```csharp
if (request.UnitPrice <= 0)
{
    return "El precio debe ser mayor que cero.";
}
```

se lee casi como una especificación:

**si** el precio es cero o negativo, **entonces** la petición no es aceptable.

No necesitamos un patrón de diseño para expresar esto.

## Resultado explícito

Abre [`ProductCreationResult.cs`](../app/src/StockFlow.Api/Products/ProductCreationResult.cs).

`TryAdd` no lanza una excepción cuando el usuario envía stock negativo. Devuelve un resultado que puede ser éxito o fallo.

¿Por qué? Porque una entrada inválida es una situación esperable. Las excepciones se reservarán para fallos excepcionales o para capas donde realmente aporten valor.

## Demostración

[EJECUTAR]

```bash
curl -i -X POST http://localhost:5073/api/products \
  -H "Content-Type: application/json" \
  -d '{"sku":"BAD-01","name":"Dato inválido","unitPrice":100,"stock":-1}'
```

La API debe responder HTTP 400 y explicar que la existencia no puede ser negativa.

Prueba ahora un SKU duplicado usando diferencias de mayúsculas y minúsculas.

## Normalización antes de comparar

StockFlow transforma el SKU antes de guardarlo:

```csharp
var normalizedSku = request.Sku.Trim().ToUpperInvariant();
```

Y compara con `StringComparison.OrdinalIgnoreCase`.

El objetivo no es presumir APIs de strings: es impedir que `MON-01` y `mon-01` se conviertan accidentalmente en dos productos distintos.

## Un detalle profesional: estado compartido

`ProductCatalog` vive como singleton. Varias solicitudes pueden llegar al mismo objeto. Por eso las operaciones sobre la lista se protegen con `lock`.

No estudiaremos concurrencia todavía. Sólo deja anotada la razón: **una colección mutable compartida exige pensar en acceso concurrente**.

Más adelante podremos reemplazar este almacenamiento en memoria por persistencia real.

## Errores comunes

### Validar sólo en la UI

Una API no puede confiar en que todos sus consumidores usan la misma interfaz.

### Lanzar excepciones por cada error de usuario

Puede funcionar, pero mezcla flujo esperado con situaciones excepcionales y dificulta razonar sobre el comportamiento.

### Corregir silenciosamente datos peligrosos

Normalizar espacios o mayúsculas es razonable. Convertir un precio negativo en positivo sin avisar no lo es.

## Buenas prácticas

- valida cerca de la frontera donde la regla puede aplicarse de forma consistente;
- devuelve mensajes accionables;
- no registres datos sensibles sólo para diagnosticar una validación;
- normaliza únicamente cuando la transformación conserva el significado esperado;
- diferencia error esperado de fallo excepcional.

## Tu turno

Agrega una regla: el SKU debe tener al menos tres caracteres después de `Trim()`.

No mires una solución. Ejecuta manualmente dos casos:

1. `AB` debe fallar;
2. `ABC` debe poder continuar.

## Cómo comprobar tu solución

Además de probar con `curl`, en la siguiente lección convertirás esa regla en una prueba automática.

## Reflexión

Si mañana cambiamos HTTP por una cola de mensajes, ¿qué reglas deberían seguir funcionando sin reescribirse? Esa pregunta nos ayuda a decidir qué pertenece a la lógica de negocio y qué pertenece al transporte.

## Resumen

- datos bien formados pueden seguir siendo inválidos;
- `if` expresa reglas claras sin ceremonia;
- `string?` hace explícita la posibilidad de ausencia;
- los errores previsibles pueden representarse como resultados;
- la concurrencia importa cuando compartimos estado mutable.

## Siguiente paso

En la [Lección 04](04-pruebas-y-checkpoint.md) protegeremos estas reglas con pruebas automáticas y harás el primer checkpoint sin receta paso a paso.

## Referencias

- [Nullable reference types](https://learn.microsoft.com/dotnet/csharp/nullable-references)
- [Sentencia `if`](https://learn.microsoft.com/dotnet/csharp/language-reference/statements/selection-statements)

# Solución de referencia — Evaluación final de C#

No existe una única solución correcta. Esta referencia muestra una dirección pequeña que respeta las fronteras aprendidas en el curso.

## 1. Lectura de arquitectura

Una explicación razonable identifica `Program.cs` como composición/transporte HTTP, `OrderService` como coordinación del caso de uso, `IOrderRepository` como frontera de persistencia y `ProductCatalog` como reglas de catálogo/inventario ejecutables sin infraestructura HTTP.

## 2. Modelo de factura educativa

Una opción mínima:

```csharp
public sealed record Invoice(
    Guid Id,
    Guid OrderId,
    DateTimeOffset IssuedAt,
    IReadOnlyList<InvoiceLine> Lines,
    decimal Total);

public sealed record InvoiceLine(
    string Description,
    int Quantity,
    decimal UnitPrice,
    decimal LineTotal);
```

La factura puede ser una proyección inmutable de un pedido. No necesita copiar reglas fiscales que StockFlow no pretende modelar.

Una implementación pequeña puede crear `InvoiceService`, recibir el acceso a pedidos y `TimeProvider`, buscar el pedido y mapear sus líneas. Si decides persistir facturas, introduce una frontera explícita en vez de escribir SQL desde el endpoint.

## 3. Endpoint

Una forma coherente sería:

`POST /api/orders/{orderId}/invoice`

Resultados esperables:

- `201 Created` con la factura;
- `404 ProblemDetails` si el pedido no existe;
- `500 ProblemDetails` genérico ante una excepción inesperada.

También es válido modelar `/api/invoices` si puedes explicar cómo llega el `OrderId` y mantienes el contrato claro.

## 4. Bug de SKU nulo

La corrección debe ocurrir antes de cualquier `Trim()` o normalización.

Por ejemplo, una frontera de dominio puede comprobar:

```csharp
if (requests.Any(request => string.IsNullOrWhiteSpace(request.Sku)))
{
    return StockReservationResult.Failure("Todas las líneas deben indicar un SKU.");
}
```

También puedes validar antes en `OrderService`; mantener una defensa razonable dentro de `ProductCatalog.TryReserve` evita que la clase dependa de callers perfectos.

La prueba importante envía o construye una línea con SKU nulo y demuestra que el resultado es controlado. Si la pruebas por HTTP, debe ser un `400`, no un `500`.

## 5. Pruebas

Una combinación equilibrada podría usar:

- prueba pequeña de servicio para mapear pedido → factura;
- prueba de servicio/dominio para SKU nulo;
- una prueba HTTP para el contrato `404` o `201` de facturación.

No necesitas duplicar todos los casos en todos los niveles.

## 6. Fuente oficial

Ejemplos válidos incluyen documentación oficial de ASP.NET Core sobre Minimal APIs, manejo de errores o integration testing. La evidencia es explicar qué decisión apoyó, no sólo pegar un enlace.

## 7. Mejora transaccional

El estado actual descuenta inventario en memoria y luego persiste el pedido. La compensación restaura inventario si guardar falla, pero no es una transacción durable entre procesos.

Si productos, existencias y pedidos vivieran en SQLite, una mejora razonable sería ejecutar validación de stock, decremento y alta del pedido dentro de una sola transacción de base de datos. Eso mejora atomicidad, pero obliga a mover parte de la coordinación hacia una frontera de persistencia capaz de representar la unidad de trabajo y gestionar concurrencia.

## Qué comparar con tu solución

Pregunta:

- ¿puedo explicar cada clase nueva?
- ¿agregué abstracciones porque resolvían un problema o por decoración?
- ¿mis tests prueban comportamiento observable?
- ¿el bug tiene una prueba de regresión?
- ¿el cliente recibe errores seguros?
- ¿sé decir qué todavía no es producción?

Si tu solución difiere pero responde bien esas preguntas y satisface la rúbrica, no necesitas cambiarla sólo para parecerte a esta referencia.

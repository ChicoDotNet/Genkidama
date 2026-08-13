# Evaluación final — StockFlow: facturación simplificada y robustez

Trabaja sobre la aplicación resultante de las 16 lecciones y los checkpoints. No hay receta paso a paso.

## 1. Comprensión de la base existente

Antes de editar, escribe un párrafo breve explicando:

- responsabilidad de `Program.cs`;
- responsabilidad de `OrderService`;
- para qué existe `IOrderRepository`;
- por qué `ProductCatalog` puede probarse sin HTTP ni SQLite.

## 2. Nueva funcionalidad: factura simplificada

Agrega la capacidad de emitir un comprobante interno a partir de un pedido existente.

El modelo mínimo debe conservar:

- `Id` propio;
- `OrderId`;
- fecha/hora de emisión;
- líneas con descripción, cantidad, precio unitario e importe;
- total.

No implementes CFDI, IVA, timbrado, folios fiscales ni reglas tributarias. Es una factura **educativa**, no un documento fiscal mexicano.

Expón una operación HTTP razonable para emitirla. Si el pedido no existe, devuelve un contrato de error coherente con el resto de StockFlow.

Decide si necesitas una frontera de repositorio nueva o si una solución más pequeña basta para esta evaluación. Explica tu decisión.

## 3. Bugfix: JSON válido que puede traer `null`

Un cliente puede enviar una línea de pedido parecida a:

```json
{
  "sku": null,
  "quantity": 1
}
```

Aunque el tipo C# declare `string`, los datos externos no deben asumirse correctos sólo por nullable annotations.

Reproduce el fallo actual y corrígelo para que una entrada así produzca un error de cliente controlado, no una excepción inesperada.

Agrega una prueba que falle antes de tu corrección y pase después.

## 4. Contrato de errores

La funcionalidad nueva debe distinguir al menos:

- solicitud inválida;
- pedido inexistente;
- error inesperado.

No devuelvas stack traces, SQL ni `exception.Message` como contrato público.

## 5. Pruebas

Agrega pruebas en los niveles que consideres adecuados. Como mínimo demuestra:

- factura creada desde un pedido válido;
- pedido inexistente;
- regresión de `sku: null`.

No conviertas cada caso en una prueba HTTP si una prueba más pequeña comunica mejor la regla.

## 6. Documentación oficial

Consulta al menos una fuente oficial de Microsoft relacionada con una decisión de tu implementación: Minimal APIs, ProblemDetails, testing, logging, DI o JSON.

En tu nota de entrega indica qué consultaste y qué decisión ayudó a tomar. No copies grandes fragmentos.

## 7. Diseño de siguiente mejora

Sin implementarla, explica cómo harías consistente el inventario y el pedido bajo una transacción real si ambos se persistieran en SQLite.

Describe el problema que resuelve y el trade-off principal. No se evalúa que nombres un patrón famoso.

## Validación final

```bash
dotnet build app/src/StockFlow.Api/StockFlow.Api.csproj --configuration Release
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj --configuration Release
```

Después ejecuta la aplicación y demuestra manualmente el camino feliz de tu factura.

## Entrega

Incluye código, tests y una nota breve con:

- arquitectura entendida;
- decisiones tomadas;
- bug corregido;
- fuente oficial consultada;
- mejora futura propuesta.

Evalúate con [`../rubric.md`](../rubric.md) antes de abrir la [`solución de referencia`](../solutions/evaluacion-final.md).

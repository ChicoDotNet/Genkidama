# Checkpoint 01 — Agrega una regla de inventario sin receta

Has recibido una regla nueva de negocio:

> Un producto no puede darse de alta con más de 10,000 unidades de existencia inicial. Cantidades mayores probablemente indican un error de captura o importación y deben rechazarse explícitamente.

## Tu trabajo

Modifica StockFlow para cumplir la regla.

Criterios observables:

- `stock: 10000` sigue siendo válido;
- `stock: 10001` falla;
- el mensaje explica el problema;
- existe al menos una prueba automatizada nueva;
- las pruebas existentes siguen pasando;
- el endpoint devuelve HTTP 400 para el caso inválido.

No se indica qué archivo ni qué línea cambiar.

## Evidencia mínima

Conserva:

1. salida de `dotnet test`;
2. la petición HTTP inválida y su respuesta;
3. una frase explicando por qué colocaste la regla donde la colocaste.

## Reto adicional

¿Qué cambiarías si el máximo dependiera de la categoría de producto? No lo implementes todavía; describe una opción que no obligue a llenar `Program.cs` de condiciones.

Cuando hayas terminado un intento real, consulta la [solución de referencia](../solutions/checkpoint-01.md).

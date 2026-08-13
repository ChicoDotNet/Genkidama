# Lección 08 — Composición, reglas del dominio y segundo checkpoint

## Qué vas a conseguir

Vas a estudiar una propiedad profesional del primer pedido: **o se reserva completo o no se modifica nada**. Después introducirás una regla nueva por tu cuenta y deberás demostrarla con pruebas.

## El problema: un pedido parcialmente descontado

Imagina este pedido:

- 2 unidades de `MOU-01`, disponibles;
- 10 unidades de `LAP-001`, no disponibles.

Una implementación ingenua podría descontar primero los mouse y descubrir después que faltan laptops. El endpoint devuelve error, pero el inventario ya cambió.

Eso es peor que un simple mensaje equivocado: el estado del negocio quedó inconsistente.

## Validar primero, modificar después

`ProductCatalog.TryReserve` trabaja bajo una sección protegida y hace dos recorridos conceptuales.

Primero comprueba que **todas** las solicitudes pueden cumplirse. Sólo después reemplaza los productos con nuevas existencias.

La prueba:

`TryReserve_WhenAnyLineHasInsufficientStock_DoesNotChangeAnyProduct`

protege ese comportamiento.

No estamos implementando todavía una transacción de base de datos; estamos aprendiendo la propiedad que la futura transacción deberá conservar.

## Inmutabilidad local con `record` y `with`

`Product` es un record. Para cambiar su stock no alteramos una propiedad arbitrariamente:

```csharp
_products[index] = product with
{
    Stock = product.Stock - request.Quantity
};
```

`with` crea una nueva instancia basada en la anterior. La lista cambia la referencia que guarda; el producto previo no se reescribe en sitio.

Esto no convierte automáticamente todo el sistema en inmutable, pero hace el cambio más explícito.

## Encapsulación

¿Por qué `OrderService` no hace `_products[index]` directamente?

Porque `_products` es detalle interno de `ProductCatalog`. Si otro componente pudiera modificarlo, ninguna regla de inventario tendría un único lugar confiable.

Encapsular no significa esconder por esconder. Significa que una responsabilidad gobierna su estado y ofrece operaciones con intención.

## Una prueba de colaboración

En `OrderServiceTests` comprobamos un efecto que cruza objetos:

- se crea un pedido;
- se calcula el total;
- se usa el reloj recibido;
- se reduce el inventario.

Eso sigue siendo una prueba rápida y local: no necesita servidor, red ni base de datos.

## Checkpoint 02

Ahora resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md).

Nueva regla del negocio:

> Una sola línea de pedido no puede solicitar más de 10 unidades. Si la regla falla, no debe cambiar el inventario.

No se indica qué clase ni qué método debes modificar.

Tu trabajo es:

1. localizar la responsabilidad correcta;
2. implementar la regla;
3. escribir al menos una prueba de rechazo;
4. demostrar que el inventario permanece igual;
5. conservar las pruebas existentes en verde.

Sólo después compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).

## Cómo comprobar tu solución

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Además intenta crear el pedido por HTTP y observa el mensaje devuelto.

## Errores comunes

### Validar sólo en `Program.cs`

La regla desaparecería si mañana `OrderService` es llamado desde otro transporte o una prueba. Las reglas de negocio deben vivir donde se ejecuta el caso de uso.

### Descontar y después devolver error

Un error no deshace automáticamente mutaciones previas. Piensa primero en qué propiedades del estado deben mantenerse.

### Probar únicamente el mensaje

El mensaje importa, pero la propiedad crítica es que el inventario no cambie.

## Buenas prácticas

- formula reglas como comportamiento observable;
- coloca la regla cerca de la responsabilidad que la gobierna;
- protege invariantes con pruebas;
- evita exponer colecciones internas mutables;
- separa validación de modificación cuando necesitas atomicidad.

## Reflexión

¿Qué cambiará cuando StockFlow tenga dos procesos concurrentes y SQLite? El mecanismo será diferente, pero la regla “todo o nada” seguirá siendo la misma.

## Resumen

- composición permite que objetos pequeños colaboren;
- encapsulación protege quién puede modificar estado;
- validar todo antes de modificar evita estados parciales en este modelo en memoria;
- los checkpoints miden si puedes localizar y cambiar una regla sin seguir instrucciones línea por línea.

## Siguiente paso

El siguiente bloque hará más preciso el contrato HTTP, y después reemplazará la memoria volátil por SQLite. La persistencia aparecerá porque ahora sí tenemos datos que perder al reiniciar.

## Referencias

- [Records en C#](https://learn.microsoft.com/dotnet/csharp/language-reference/builtin-types/record)
- [Encapsulación](https://learn.microsoft.com/dotnet/csharp/fundamentals/tutorials/classes)

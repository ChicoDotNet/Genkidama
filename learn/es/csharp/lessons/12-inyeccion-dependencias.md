# Lección 12 — Inyección de dependencias sin magia

## Qué vas a conseguir

Vas a entender por qué `OrderService` recibe sus colaboradores y cómo ASP.NET Core conecta la implementación SQLite en producción mientras los tests usan memoria.

## El problema

Si `OrderService` escribiera `new SqliteOrderRepository(...)` dentro de cada operación, las reglas de pedido conocerían archivos, connection strings y SQLite. Probar un pedido exigiría tocar disco.

## Concepto

Inyección de dependencias significa que un objeto recibe lo que necesita en lugar de construirlo escondidamente. `IOrderRepository` es el contrato; `SqliteOrderRepository` e `InMemoryOrderRepository` son implementaciones.

## Demostración

[EN PANTALLA]

```csharp
public sealed class OrderService(
    ProductCatalog catalog,
    IOrderRepository repository,
    TimeProvider timeProvider)
```

Luego mira `Program.cs`:

```csharp
builder.Services.AddSingleton<IOrderRepository>(_ =>
    new SqliteOrderRepository("Data Source=stockflow.db"));
```

Y compara con un test, donde se pasa `new InMemoryOrderRepository()` directamente.

## Código real

También inyectamos `TimeProvider`: una prueba puede fijar la hora sin modificar el reloj del sistema. Una dependencia controlable vuelve determinista un comportamiento que de otro modo cambia cada ejecución.

## Qué acaba de pasar

El contenedor de ASP.NET Core no crea arquitectura por sí solo. La decisión importante ocurrió antes: `OrderService` depende de capacidades pequeñas y explícitas.

## Errores comunes

- crear una interfaz para cada clase aunque sólo exista una razón estética;
- usar el contenedor como localizador global de servicios;
- esconder dependencias detrás de métodos estáticos;
- confundir “DI” con un framework específico.

## Buenas prácticas

Introduce una abstracción cuando separa una frontera real: almacenamiento, reloj, red o similar. La lógica de negocio debería seguir siendo legible sin conocer el contenedor.

## Tu turno

Resuelve el [Checkpoint 03](../exercises/checkpoint-03.md). Ahora tendrás que extender contrato, implementaciones, servicio y HTTP sin una receta línea por línea.

## Cómo comprobar

Ejecuta todos los tests y realiza una consulta manual después de reiniciar StockFlow.

## Reto adicional

¿Qué cambiaría si mañana los pedidos vivieran en PostgreSQL? Enumera los archivos que deberían cambiar y los que no.

## Resumen

DI hace visibles las dependencias, facilita pruebas y reduce acoplamiento cuando se aplica a fronteras que de verdad cambian o realizan I/O.

## Siguiente paso

En la [Lección 13](13-pruebas-de-endpoints.md) probaremos endpoints completos y regresiones HTTP, no sólo servicios aislados.

## Referencias

- [Dependency injection en .NET](https://learn.microsoft.com/dotnet/core/extensions/dependency-injection)

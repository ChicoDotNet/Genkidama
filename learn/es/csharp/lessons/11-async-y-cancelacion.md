# Lección 11 — I/O asíncrono y cancelación

## Qué vas a conseguir

Vas a seguir el recorrido `HTTP → servicio → repositorio → SQLite` y entender por qué ahora devuelve `Task` y recibe `CancellationToken`.

## El problema

Abrir un archivo o esperar a una base de datos no usa CPU todo el tiempo. Bloquear un hilo mientras espera limita cuántas solicitudes puede atender un servidor.

## Concepto

`async` y `await` permiten suspender una operación mientras el I/O progresa. No hacen mágicamente más rápido al disco; permiten usar mejor los recursos del proceso.

`CancellationToken` expresa otra idea: si el cliente abandona la solicitud, el trabajo pendiente puede dejar de ser útil.

## Demostración

Sigue `GetAllAsync` desde `Program.cs` hasta `SqliteOrderRepository`. Observa que el token recibido por ASP.NET Core se pasa a `OpenAsync`, `ExecuteReaderAsync` y `ReadAsync`.

## Código real

```csharp
app.MapGet("/api/orders", async (OrderService orders, CancellationToken cancellationToken) =>
    Results.Ok(await orders.GetAllAsync(cancellationToken)));
```

No hay `Task.Run`. El trabajo ya es I/O asíncrono; envolverlo en otro hilo sólo añadiría ruido.

## Qué acaba de pasar

La asincronía atraviesa las fronteras que realmente esperan I/O. El catálogo de productos sigue síncrono porque opera sobre una lista en memoria y un lock corto.

## Errores comunes

- usar `.Result` o `.Wait()` por comodidad;
- marcar métodos `async` sin ningún `await` útil;
- crear `Task.Run` alrededor de llamadas async;
- aceptar un token y no propagarlo.

## Buenas prácticas

Haz async el camino de I/O de extremo a extremo. Mantén síncrona la lógica pura cuando no obtiene ningún beneficio de convertirse en `Task`.

## Tu turno

Busca cada aparición de `CancellationToken` en el repositorio de pedidos y dibuja las llamadas en orden. Identifica dónde se comprobaría cancelación en la implementación en memoria.

## Cómo comprobar

Las pruebas async deben terminar verdes y el endpoint `/api/orders` debe seguir devolviendo el historial.

## Reto adicional

Explica por qué cancelar después de descontar stock, pero antes de persistir, sería delicado. Localiza la compensación que restaura inventario cuando guardar falla.

## Resumen

`async` es una herramienta de I/O y escalabilidad, no una palabra que deba aparecer en cada método. La cancelación debe viajar hasta el recurso que puede detener trabajo.

## Siguiente paso

Ya tenemos dos repositorios intercambiables. La siguiente lección pondrá nombre a la técnica que permite elegir uno sin acoplar `OrderService` a SQLite.

## Referencias

- [Programación asíncrona con async y await](https://learn.microsoft.com/dotnet/csharp/asynchronous-programming/)
- [CancellationToken](https://learn.microsoft.com/dotnet/api/system.threading.cancellationtoken)

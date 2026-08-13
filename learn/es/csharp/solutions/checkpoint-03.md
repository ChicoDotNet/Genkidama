# Solución de referencia — Checkpoint 03

Una solución razonable amplía el contrato con:

```csharp
Task<Order?> GetByIdAsync(Guid id, CancellationToken cancellationToken = default);
```

En memoria puede usarse `FirstOrDefault` dentro del mismo lock que protege la lista. En SQLite usa una consulta parametrizada con `WHERE Id = $id`; reutiliza un método privado que convierta la fila a `Order` para no duplicar parseo entre `GetAllAsync` y `GetByIdAsync`.

`OrderService` sólo delega la consulta al repositorio. El endpoint traduce `null` a `404 ProblemDetails` y un pedido encontrado a `200`.

Una prueba útil escribe un pedido en el repositorio, lo consulta por su id y comprueba que un Guid distinto devuelve `null`. Otra prueba puede cubrir la traducción HTTP cuando el curso llegue a pruebas de endpoint en la lección 13.

La decisión importante no es copiar exactamente estos nombres: es mantener el camino async, parametrizar SQL y conservar la separación `HTTP → servicio → repositorio`.

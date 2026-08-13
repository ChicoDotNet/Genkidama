# Lección 10 — Persistencia local con SQLite

## Qué vas a conseguir

Vas a guardar el historial de pedidos en un archivo SQLite y comprobar que sobrevive a un reinicio de StockFlow.

## El problema

El pedido que acabas de crear desaparece al detener la API. Una aplicación de negocio que olvida todo al reiniciar es una demostración, no una herramienta utilizable.

## Concepto

SQLite es una base relacional embebida: la aplicación abre un archivo local y ejecuta SQL sin instalar un servidor. StockFlow usa `Microsoft.Data.Sqlite`, el proveedor ADO.NET ligero de Microsoft, sin introducir todavía un ORM.

## Demostración

Revisa `SqliteOrderRepository.cs`. Hay tres operaciones importantes:

1. crear la tabla si todavía no existe;
2. insertar un pedido parametrizando valores;
3. leer filas y reconstruir objetos `Order`.

Los parámetros `$id`, `$createdAt`, etc. no son decoración: evitan concatenar datos del usuario dentro de SQL.

[EJECUTAR]

```bash
dotnet run --project app/src/StockFlow.Api/StockFlow.Api.csproj --urls http://localhost:5073
```

Crea un pedido, consulta `/api/orders`, detén el proceso, vuelve a iniciarlo y consulta otra vez. El pedido debe seguir ahí en `stockflow.db`.

## Código real

`IOrderRepository` define lo que necesita `OrderService`; `SqliteOrderRepository` decide cómo persistirlo. Las líneas se serializan como JSON dentro de esta primera tabla para mantener el modelo relacional pequeño mientras el alumno entiende la frontera de persistencia.

## Qué acaba de pasar

La aplicación ganó estado durable sin convertir toda la lección en Entity Framework. También apareció un nuevo tipo de fallo: disco, archivo bloqueado o SQL pueden fallar.

## Errores comunes

- concatenar SQL con interpolación de strings;
- guardar dinero como `double` sin pensar en precisión;
- asumir que “base de datos” elimina la necesidad de manejar errores;
- probar sólo contra una instancia en memoria y nunca reabrir el archivo.

## Buenas prácticas

El test `SqliteOrderRepositoryTests` escribe con una instancia y lee con otra. Eso demuestra persistencia, no sólo que una lista conserva objetos.

## Tu turno

Agrega un segundo pedido manualmente, reinicia la aplicación y verifica que ambos aparecen ordenados por fecha descendente.

## Cómo comprobar

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

## Reto adicional

¿Por qué el inventario todavía puede reiniciarse aunque los pedidos permanezcan? Describe qué consistencia adicional exigirías antes de llamar a StockFlow “producción”.

## Resumen

Persistir no significa añadir una dependencia y ya: significa definir una frontera, representar datos, manejar recursos y demostrar que sobreviven a otro proceso.

## Siguiente paso

Leer y escribir almacenamiento es I/O. La siguiente lección explica por qué esos caminos se vuelven `async` y cómo se cancela trabajo que ya no interesa.

## Referencias

- [Microsoft.Data.Sqlite](https://learn.microsoft.com/dotnet/standard/data/sqlite/)

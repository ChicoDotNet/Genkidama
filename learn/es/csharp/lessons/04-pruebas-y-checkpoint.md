# Lección 04 — Pruebas y primer checkpoint profesional

## Qué vas a conseguir

Vas a ejecutar pruebas automáticas sobre reglas reales de StockFlow y después modificarás el código sin una receta exacta.

Este es el primer punto en el que dejamos de preguntar “¿seguiste el tutorial?” y empezamos a preguntar “¿puedes cambiar el sistema sin romper lo que ya funcionaba?”.

## El problema

Hoy sabemos manualmente que:

- un producto válido se agrega;
- un stock negativo se rechaza;
- un SKU duplicado se rechaza.

Si comprobáramos esos casos con `curl` después de cada cambio, pronto dejaríamos de hacerlo.

## Concepto: una prueba automatizada

Abre [`ProductCatalogTests.cs`](../app/tests/StockFlow.Api.Tests/ProductCatalogTests.cs).

Una prueba tiene tres movimientos fáciles de reconocer:

1. **Arrange:** prepara objetos y datos;
2. **Act:** ejecuta el comportamiento;
3. **Assert:** comprueba el resultado.

No es obligatorio escribir comentarios con esos nombres. Lo importante es que la intención sea visible.

## Demostración

[EJECUTAR]

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Debes ver las pruebas en verde.

Ahora cambia temporalmente esta regla:

```csharp
if (request.Stock < 0)
```

por una condición incorrecta. Ejecuta de nuevo los tests y observa cuál falla. Después revierte el defecto.

El valor no está en “tener verde”. Está en que una regla importante puede avisarte cuando la rompes.

## ¿Por qué MSTest?

MSTest es un framework de pruebas soportado por Microsoft y encaja con las reglas del repositorio Genkidama. No necesitas aprender tres frameworks de testing para comprender pruebas unitarias.

## Tests y diseño

`ProductCatalog` puede probarse sin iniciar un servidor HTTP. Eso ocurre porque las reglas de productos no están escondidas dentro del endpoint.

Esta separación todavía es pequeña:

- `Program.cs` traduce HTTP a llamadas;
- `ProductCatalog` aplica reglas;
- los records representan datos.

No necesitamos bautizar esto como una arquitectura sofisticada. La separación existe porque nos ayuda a cambiar y probar.

## Buenas pruebas

Una prueba útil:

- comprueba comportamiento observable;
- tiene un nombre que explica escenario y resultado;
- falla por una razón relevante;
- no depende de orden aleatorio, red o reloj si no es necesario;
- evita duplicar la implementación dentro del test.

## Tu turno: primera prueba nueva

Implementa la regla de la lección anterior: el SKU, después de `Trim()`, debe tener al menos tres caracteres.

Antes o después del código —elige conscientemente— agrega una prueba que demuestre el comportamiento.

El resultado esperado para `" AB "` es un fallo con un mensaje útil.

## Cómo comprobar tu solución

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Después ejecuta StockFlow y verifica que el endpoint también devuelve HTTP 400.

## Checkpoint 01

Ahora resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md).

No contiene instrucciones línea por línea. Tendrás que leer el código, decidir dónde cambia la regla y protegerla con una prueba.

No abras la solución hasta haber realizado un intento real.

## Errores comunes

### Cambiar el test para que acepte el bug

Un test no es un obstáculo que hay que silenciar. Si el requisito cambió, explica primero por qué; si no cambió, corrige el producto.

### Probar métodos privados directamente

Prefiere comprobar el comportamiento público que esos detalles internos hacen posible.

### Una prueba enorme para todo

Cuando falla, no sabes qué regla se rompió. Empieza con escenarios pequeños y concretos.

## Nota para instructor

Aquí conviene provocar un fallo intencional, leer el mensaje de MSTest en pantalla y pedir al grupo que prediga qué línea del producto debería cambiar antes de editar nada.

## Reflexión

¿Podrías agregar otra regla al catálogo sin tocar `Program.cs`? Si la respuesta es sí, ya estás empezando a separar transporte de reglas.

## Resumen

- una prueba convierte una expectativa en una comprobación repetible;
- el diseño mejora cuando la lógica puede ejecutarse sin infraestructura innecesaria;
- verde no es la meta: la meta es detectar regresiones relevantes;
- el checkpoint mide modificación autónoma, no memoria del tutorial.

## Siguiente paso

La siguiente lección introducirá consultas, colecciones y filtros sobre un problema visible: encontrar productos sin descargar todo el inventario mentalmente.

## Referencias

- [Introducción a pruebas unitarias con MSTest](https://learn.microsoft.com/dotnet/core/testing/unit-testing-mstest-intro)

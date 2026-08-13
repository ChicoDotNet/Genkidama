# Curso de C# desde cero — Construye una API de inventario, pedidos y facturación

Este curso enseña C# desde cero construyendo **StockFlow**, una API local para una pequeña empresa. No empieza con horas de sintaxis: en la primera lección levantas un servidor y ves datos reales. Después la misma aplicación introduce tipos, colecciones, LINQ, validación, pedidos, persistencia, asincronía, pruebas, contratos, observabilidad y arquitectura.

## ¿Qué es C# y para qué se utiliza?

C# es el lenguaje principal del ecosistema .NET. Se utiliza habitualmente en APIs y backend con ASP.NET Core, software empresarial, aplicaciones de escritorio, servicios cloud, herramientas y otros tipos de aplicaciones .NET.

## ¿Puedo aprenderlo desde cero?

Sí. No necesitas haber programado antes. El curso presupone únicamente que puedes instalar software, abrir una terminal y editar archivos en VS Code.

## ¿Qué vas a construir?

**StockFlow** crecerá durante 17 lecciones hasta administrar productos, existencias, pedidos y facturación simplificada, con persistencia local, manejo explícito de errores, pruebas, documentación OpenAPI y hardening básico.

La aplicación canónica vive en [`app/`](app/) y no depende del CLI principal de Genkidama.

## Tooling verificado

La línea elegida es **.NET 10 LTS / C# 14**. La metadata exacta vive en [`course.yml`](course.yml). Para SQLite se usa `Microsoft.Data.Sqlite 10.0.10`; para pruebas HTTP, `Microsoft.AspNetCore.Mvc.Testing 10.0.10`; y para documentación ejecutable, `Microsoft.AspNetCore.OpenApi 10.0.10`.

Objetivo de uso: Windows 11 + PowerShell + VS Code y Linux actual + bash + VS Code.

## Instalar

Instala el SDK de .NET 10 y comprueba:

```bash
dotnet --version
```

## Build

```bash
dotnet build app/src/StockFlow.Api/StockFlow.Api.csproj
```

## Test

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

## Run

```bash
dotnet run --project app/src/StockFlow.Api/StockFlow.Api.csproj --urls http://localhost:5073
```

Después abre `http://localhost:5073/health` o consulta `http://localhost:5073/api/products`.

## Qué sabrás hacer al terminar

Deberías poder leer y escribir C# sencillo e idiomático; modelar datos; trabajar con colecciones y LINQ; crear endpoints; separar reglas de I/O; manejar errores; persistir datos; usar async/cancelación; escribir pruebas unitarias y HTTP; documentar contratos; diagnosticar problemas; aplicar hardening básico y extender una base existente sin receta paso a paso.

## Ruta del curso

Estado actual: **16 de 17 lecciones implementadas**.

1. [Tu primera API en ejecución](lessons/01-tu-primera-api.md)
2. [Productos, variables y tipos que representan negocio](lessons/02-productos-y-tipos.md)
3. [Validación y errores que el usuario puede entender](lessons/03-validacion-y-errores.md)
4. [Pruebas y primer checkpoint profesional](lessons/04-pruebas-y-checkpoint.md)
5. [Consultas, colecciones y filtros](lessons/05-consultas-colecciones-y-filtros.md)
6. [Funciones, LINQ y transformaciones](lessons/06-linq-y-transformaciones.md)
7. [El primer pedido](lessons/07-el-primer-pedido.md)
8. [Clases, composición y reglas de dominio](lessons/08-composicion-reglas-y-checkpoint.md)
9. [Errores HTTP que forman parte del contrato](lessons/09-contratos-http.md)
10. [Persistencia local con SQLite](lessons/10-persistencia-sqlite.md)
11. [I/O asíncrono y cancelación](lessons/11-async-y-cancelacion.md)
12. [Inyección de dependencias sin magia](lessons/12-inyeccion-dependencias.md)
13. [Pruebas de endpoints y regresiones HTTP](lessons/13-pruebas-de-endpoints.md)
14. [Documentación de API y contratos con OpenAPI](lessons/14-openapi-y-contratos.md)
15. [Debugging, logging y diagnóstico](lessons/15-logging-y-diagnostico.md)
16. [Seguridad básica y hardening](lessons/16-seguridad-y-hardening.md)
17. Evaluación final: extender StockFlow sin receta

## Checkpoints

- después de la lección 4: [`checkpoint-01`](exercises/checkpoint-01.md);
- después de la lección 8: [`checkpoint-02`](exercises/checkpoint-02.md);
- después de la lección 12: [`checkpoint-03`](exercises/checkpoint-03.md);
- después de la lección 16: [`checkpoint-04`](exercises/checkpoint-04.md), con [`solución de referencia`](solutions/checkpoint-04.md).

## ¿Qué tipo de trabajo utiliza estas habilidades?

Las habilidades del curso aparecen en desarrollo backend/.NET, APIs empresariales, mantenimiento y evolución de aplicaciones de negocio y automatización sobre .NET. El curso busca una base demostrable; no garantiza contratación ni reemplaza práctica en equipos reales.

## Preguntas frecuentes

### ¿Necesito Visual Studio?
No. VS Code y la CLI de .NET son suficientes.

### ¿Por qué ASP.NET Core si el curso es de C#?
Porque una API pequeña vuelve visibles problemas cercanos al trabajo real sin convertir el material en un curso de frontend.

### ¿Por qué SQLite sin Entity Framework?
Porque primero queremos ver SQL, conexiones, serialización, `async` y una frontera de persistencia. Un ORM puede estudiarse después sobre fundamentos entendidos.

### ¿Persiste ya todo el inventario?
No. Persiste el historial de pedidos; el catálogo sigue en memoria. Esa limitación es intencional y sirve para discutir consistencia y próximos pasos, no para fingir que StockFlow ya es producción.

### ¿OpenAPI está expuesto en producción?
No por defecto. StockFlow lo mapea sólo en Development. La documentación generada sigue siendo parte del contrato y puede publicarse de otra forma cuando exista una necesidad real.

### ¿StockFlow ya es seguro para Internet?
No. El curso aplica hardening básico, pero no implementa autenticación, autorización ni la operación de producción completa.

### ¿Tengo que aprender Git aquí?
No. Git tendrá su propio curso.

## Glosario

- **SDK:** herramientas para compilar, ejecutar y probar .NET.
- **Endpoint:** ruta y operación HTTP que expone una capacidad.
- **Record:** tipo C# con semántica de valor útil para datos.
- **Repositorio:** frontera que guarda y recupera objetos del dominio.
- **CancellationToken:** señal cooperativa para detener trabajo que dejó de ser útil.
- **DI:** técnica para recibir dependencias en lugar de construirlas ocultamente.
- **WebApplicationFactory:** utilidad de ASP.NET Core para probar una aplicación mediante un servidor HTTP de pruebas.
- **OpenAPI:** especificación independiente del lenguaje para describir APIs HTTP.
- **ProblemDetails:** formato estándar para comunicar errores HTTP con estructura consistente.
- **Hardening:** reducción deliberada de superficie y riesgos evitables.

## Cómo hablar de este proyecto en una entrevista

Prepárate para explicar por qué empezaste en memoria, qué problema justificó SQLite, por qué `OrderService` no conoce SQL, cómo proteges el stock si guardar falla, dónde viaja `CancellationToken`, qué riesgo cubren las pruebas HTTP, por qué OpenAPI sólo se expone en Development y qué faltaría antes de operar StockFlow en Internet.

## Referencias oficiales

- [Documentación de C#](https://learn.microsoft.com/dotnet/csharp/)
- [ASP.NET Core](https://learn.microsoft.com/aspnet/core/)
- [Microsoft.Data.Sqlite](https://learn.microsoft.com/dotnet/standard/data/sqlite/)
- [Pruebas de integración en ASP.NET Core](https://learn.microsoft.com/aspnet/core/test/integration-tests)
- [OpenAPI en ASP.NET Core](https://learn.microsoft.com/aspnet/core/fundamentals/openapi/overview)
- [Logging en ASP.NET Core](https://learn.microsoft.com/aspnet/core/fundamentals/logging/)
- [Manejo de errores](https://learn.microsoft.com/aspnet/core/fundamentals/error-handling)
- [Política de soporte de .NET](https://dotnet.microsoft.com/platform/support/policy)
- [MSTest](https://learn.microsoft.com/dotnet/core/testing/unit-testing-mstest-intro)

## Siguiente paso

Si estudias el curso desde cero, empieza en la [Lección 1](lessons/01-tu-primera-api.md). Si vienes siguiendo la construcción, completa el [`checkpoint-04`](exercises/checkpoint-04.md): la siguiente lección será la evaluación final sin receta.

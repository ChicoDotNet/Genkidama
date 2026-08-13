# Curso de C# desde cero — Construye una API de inventario, pedidos y facturación

Este curso enseña C# desde cero construyendo **StockFlow**, una API local para una pequeña empresa. No empieza con seis horas de sintaxis: en la primera lección levantas un servidor, consultas un endpoint y ves datos reales. Después usamos esa aplicación para aprender tipos, colecciones, funciones, validación, diseño orientado a objetos, errores, persistencia, pruebas y arquitectura.

## ¿Qué es C# y para qué se utiliza?

C# es el lenguaje principal del ecosistema .NET. Se utiliza habitualmente en APIs y backend con ASP.NET Core, software empresarial, aplicaciones de escritorio, servicios cloud, herramientas y otros tipos de aplicaciones .NET.

## ¿Puedo aprenderlo desde cero?

Sí. No necesitas haber programado antes. El curso presupone únicamente que puedes instalar software, abrir una terminal y editar archivos en VS Code.

## ¿Qué vas a construir?

**StockFlow** crecerá durante 17 lecciones hasta administrar:

- productos y existencias;
- altas, consultas y filtros con validación;
- pedidos con líneas y totales;
- reserva atómica de inventario;
- persistencia local;
- facturación simplificada para fines educativos;
- manejo explícito de errores;
- pruebas unitarias y de API;
- documentación de uso.

La aplicación canónica vive en [`app/`](app/). No depende del CLI principal de Genkidama ni de código fuera de esta carpeta de curso.

## Tooling verificado

La línea elegida es **.NET 10 LTS / C# 14**. La metadata exacta vive en [`course.yml`](course.yml).

Objetivo de uso:

- Windows 11 + PowerShell + VS Code;
- Linux actual + bash + VS Code.

No necesitas Azure, una base de datos comercial ni un IDE de pago.

## Instalar

Instala el SDK de .NET 10 desde la documentación oficial y comprueba:

```bash
dotnet --version
```

## Build

Desde esta carpeta:

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

Después abre `http://localhost:5073/health` o ejecuta:

```bash
curl http://localhost:5073/api/products
```

## Qué sabrás hacer al terminar

Al completar el curso deberías poder:

- leer y escribir C# sencillo e idiomático;
- modelar datos con tipos propios;
- trabajar con condiciones, funciones, colecciones y LINQ;
- dividir responsabilidades en clases y servicios pequeños;
- validar entradas y representar errores de manera explícita;
- crear y modificar endpoints HTTP sencillos;
- persistir datos localmente cuando el dominio lo requiera;
- escribir y ejecutar pruebas con MSTest;
- depurar errores frecuentes;
- introducir una mejora nueva sin seguir una receta paso a paso;
- explicar la arquitectura de StockFlow en una entrevista junior.

## Ruta del curso

Estado actual: **8 de 17 lecciones implementadas**.

1. [Tu primera API en ejecución](lessons/01-tu-primera-api.md)
2. [Productos, variables y tipos que representan negocio](lessons/02-productos-y-tipos.md)
3. [Validación y errores que el usuario puede entender](lessons/03-validacion-y-errores.md)
4. [Pruebas y primer checkpoint profesional](lessons/04-pruebas-y-checkpoint.md)
5. [Consultas, colecciones y filtros](lessons/05-consultas-colecciones-y-filtros.md)
6. [Funciones, LINQ y transformaciones](lessons/06-linq-y-transformaciones.md)
7. [El primer pedido](lessons/07-el-primer-pedido.md)
8. [Composición, reglas del dominio y segundo checkpoint](lessons/08-composicion-reglas-y-checkpoint.md)
9. Errores HTTP y límites de la API
10. Persistencia local con SQLite
11. I/O asíncrono y cancelación
12. Inyección de dependencias sin magia
13. Pruebas de endpoints y regresiones
14. Documentación de API y contratos
15. Debugging, logging y diagnóstico
16. Seguridad básica y endurecimiento
17. Evaluación final: extender StockFlow sin receta

## Checkpoints

- Después de la lección 4: [`checkpoint-01`](exercises/checkpoint-01.md) y su [`solución de referencia`](solutions/checkpoint-01.md).
- Después de la lección 8: [`checkpoint-02`](exercises/checkpoint-02.md) y su [`solución de referencia`](solutions/checkpoint-02.md).

## ¿Qué tipo de trabajo utiliza estas habilidades?

Las habilidades del curso aparecen en desarrollo backend/.NET, APIs empresariales, mantenimiento y evolución de aplicaciones de negocio y automatización sobre .NET. El curso busca darte una base demostrable; no garantiza contratación ni reemplaza la práctica en equipos reales.

## Preguntas frecuentes

### ¿Necesito Visual Studio?

No. VS Code y la CLI de .NET son suficientes para el curso.

### ¿Por qué ASP.NET Core si el curso es de C#?

Porque una API pequeña convierte conceptos del lenguaje en capacidades visibles y cercanas al trabajo real. El framework se mantiene deliberadamente pequeño: la prioridad sigue siendo aprender C#.

### ¿Por qué no empezamos con Entity Framework?

Porque primero necesitas comprender los tipos, las colecciones, las reglas y los errores que luego persistiremos. La base de datos aparece cuando resuelve un problema real de StockFlow.

### ¿Tengo que aprender Git aquí?

No. Sólo necesitas obtener los archivos. Git tendrá su propio curso.

## Glosario inicial

- **SDK:** herramientas para compilar, ejecutar y probar aplicaciones .NET.
- **Runtime:** entorno que ejecuta una aplicación .NET.
- **Endpoint:** combinación de ruta y operación HTTP que expone una capacidad.
- **Record:** tipo de C# útil para representar datos con semántica de valor.
- **Servicio:** objeto que concentra una responsabilidad o conjunto pequeño de reglas.
- **LINQ:** conjunto de operadores de consulta integrados en .NET para trabajar con secuencias.
- **Composición:** construir un comportamiento mediante objetos que colaboran en lugar de una jerarquía innecesaria.
- **Test:** código que comprueba automáticamente un comportamiento esperado.

## Cómo hablar de este proyecto en una entrevista

Cuando el curso esté completo podrás explicar decisiones como:

- por qué empezaste en memoria antes de agregar persistencia;
- dónde viven las reglas de inventario y por qué no están mezcladas con HTTP;
- cómo representas un error de validación;
- por qué una reserva de varias líneas debe ser todo-o-nada;
- cómo una dependencia como el reloj se vuelve controlable en pruebas;
- qué prueba protege una regla importante;
- qué cambiarías si StockFlow tuviera múltiples instancias o miles de solicitudes concurrentes.

## Referencias oficiales

- [Documentación de C#](https://learn.microsoft.com/dotnet/csharp/)
- [ASP.NET Core](https://learn.microsoft.com/aspnet/core/)
- [Política de soporte de .NET](https://dotnet.microsoft.com/platform/support/policy)
- [MSTest](https://learn.microsoft.com/dotnet/core/testing/unit-testing-mstest-intro)

## Siguiente paso

Empieza en la [Lección 1](lessons/01-tu-primera-api.md). La meta no es memorizar sintaxis: es comprender por qué cada incremento existe y poder modificarlo por tu cuenta.

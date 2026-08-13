# Lección 16 — Seguridad básica y hardening

## Qué vas a conseguir

Vas a reducir superficie innecesaria de StockFlow y aprenderás a distinguir “funciona” de “está razonablemente endurecido para su contexto”.

## El problema

Una API puede pasar todas sus pruebas funcionales y aun así exponer información innecesaria, aceptar cuerpos enormes o devolver detalles internos cuando ocurre una excepción.

## Concepto

Hardening significa eliminar riesgos evitables sin inventar seguridad teatral. En este curso aplicamos tres controles pequeños:

1. `ProblemDetails` + exception handler para respuestas de error consistentes sin stack traces como contrato público;
2. OpenAPI sólo en Development;
3. Kestrel sin header `Server` y con límite de 1 MiB para el cuerpo de una petición.

Esto **no convierte StockFlow en una API lista para Internet**. Falta autenticación/autorización, TLS de despliegue, gestión de secretos, rate limiting según amenaza, backups y operación real.

## Demostración

[DEMO]

Revisa:

```csharp
builder.Services.AddProblemDetails();
app.UseExceptionHandler();
```

Y la configuración de Kestrel:

```csharp
options.AddServerHeader = false;
options.Limits.MaxRequestBodySize = 1_048_576;
```

## Código real

La documentación OpenAPI se condiciona por entorno:

```csharp
if (app.Environment.IsDevelopment())
{
    app.MapOpenApi();
}
```

Una herramienta útil durante desarrollo no tiene por qué estar expuesta automáticamente en producción.

## Qué acaba de pasar

No agregamos una falsa “seguridad por header”. Aplicamos controles concretos y documentamos explícitamente qué riesgos siguen abiertos.

## Errores comunes

- guardar secretos en `appsettings.json` dentro del repositorio;
- mostrar stack traces al cliente;
- usar una API key hardcodeada y llamarlo autenticación robusta;
- habilitar CORS `*` sin entender quién debe consumir la API;
- asumir que ocultar el nombre del servidor reemplaza parches y configuración correcta.

## Buenas prácticas

Usa configuración/secret stores para secretos, principio de mínimo privilegio, dependencias soportadas, validación en fronteras y una amenaza concreta para decidir controles adicionales.

## Tu turno

Identifica dos cosas que impedirían publicar StockFlow tal como está en Internet. Para cada una escribe: riesgo, impacto y el control que implementarías. Después elige **una** mejora que pueda comprobarse con una prueba automatizada y aplícala.

## Cómo comprobar

Ejecuta build y toda la suite. Después inicia StockFlow en `Production` y confirma que `/openapi/v1.json` no esté expuesto.

## Checkpoint 04

Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) antes de consultar la solución.

## Reflexión

¿Por qué una lista enorme de controles copiados de Internet puede ser menos útil que tres controles conectados con amenazas reales?

## Resumen

- seguridad no es un checkbox;
- errores, límites y exposición de tooling forman parte de la superficie;
- documentar limitaciones es ingeniería profesional;
- nunca confundas este ejercicio con una certificación de producción.

## Siguiente paso

Ya no habrá una lección guiada. La 17 es la evaluación final: leerás requisitos, modificarás StockFlow y defenderás tus decisiones sin receta.

## Referencias

- [Manejo de errores en ASP.NET Core](https://learn.microsoft.com/aspnet/core/fundamentals/error-handling)
- [Configuración de Kestrel](https://learn.microsoft.com/aspnet/core/fundamentals/servers/kestrel/options)
- [Configuración segura de secretos](https://learn.microsoft.com/aspnet/core/security/app-secrets)

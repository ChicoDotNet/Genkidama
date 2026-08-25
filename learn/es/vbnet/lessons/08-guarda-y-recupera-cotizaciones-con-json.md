# Lección 08 — Guarda y recupera cotizaciones con JSON

## Qué vas a conseguir

Vas a persistir QuoteDesk en un archivo local versionado sin mezclar `System.Text.Json` con las reglas del dominio.

## El problema

Cerrar la aplicación no debería destruir una cotización. Pero si `QuoteDraft` sabe abrir archivos, serializar JSON y aplicar reglas, pierde una responsabilidad clara y se vuelve más difícil de probar.

## Concepto

`QuoteFileStore` vive en el borde de I/O. Convierte una cotización a un DTO versionado, escribe primero a un archivo temporal y después reemplaza el destino. Al cargar, valida la versión y reconstruye el dominio pasando otra vez por sus contratos.

## Código real

Ver implementación: [QuoteFileStore.vb](../app/QuoteDesk.Core/QuoteFileStore.vb)

## Errores comunes

- Serializar directamente campos privados como contrato eterno.
- Ignorar una versión futura del archivo.
- Sobrescribir el archivo final antes de tener completo el nuevo contenido.
- Capturar todas las excepciones y devolver una cotización vacía.

## Tu turno

Guarda una cotización aprobada en una ruta temporal, vuelve a cargarla y compara cliente, estado, partidas y total.

## Cómo comprobar tu solución

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

## Solución

Después de intentarlo, revisa `QuoteWorkflowTests.FileStore_RoundTripsApprovedQuote`.

## Resumen

El dominio sigue portable; JSON y archivos están en una frontera explícita con errores visibles y schema versionado.

## Siguiente paso

Completa el Checkpoint 02 para editar, aprobar, persistir y facturar una misma cotización. Después continúa con [Lección 09 — Lista y busca cotizaciones guardadas](09-lista-y-busca-cotizaciones-guardadas.md).

## Referencias

- [System.Text.Json overview](https://learn.microsoft.com/dotnet/standard/serialization/system-text-json/overview)
- [File.Move](https://learn.microsoft.com/dotnet/api/system.io.file.move)

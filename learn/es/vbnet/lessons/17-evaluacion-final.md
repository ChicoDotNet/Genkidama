# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir

Demostrarás que puedes leer, modificar, probar, diagnosticar y explicar QuoteDesk sin seguir una receta de archivos o métodos.

## Antes de empezar

Completa el Checkpoint 04 y ejecuta desde `app/`:

```powershell
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
dotnet build .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release
```

## El problema

Un pequeño negocio usa QuoteDesk para preparar cotizaciones y generar facturas. Te pide una evolución limitada, pero no acepta perder compatibilidad con documentos existentes, ocultar errores operativos, filtrar datos del cliente en diagnóstico ni mover reglas de negocio a eventos WinForms.

No recibirás una lista de archivos que debas editar.

## Concepto

Una tarea junior real exige **leer → formular una hipótesis → probar → implementar → verificar → explicar**. Puedes consultar el compilador, las pruebas, las lecciones y documentación oficial.

## Código real

Abre la [evaluación final](../exercises/evaluacion-final.md). No consultes la solución hasta completar un intento serio.

## Tu turno

Resuelve las historias A–F, conserva `Option Strict On`, añade pruebas de regresión y prepara una explicación de cinco minutos sobre dominio, presenter, persistencia, diagnóstico, backup y entrega.

## Cómo comprobar

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
dotnet build .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release
dotnet publish .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release -r win-x64 --self-contained false -o .\publish\quotedesk
```

Usa la [rúbrica final](../exercises/rubrica-final.md) para autoevaluarte.

## Solución enlazada

Sólo después de tu intento, compara con la [solución de referencia](../solutions/evaluacion-final.md). No exige código idéntico.

## Cómo hablar de este proyecto en una entrevista

Explica primero el problema empresarial: cotizaciones editables que se aprueban, congelan, persisten y producen facturas inmutables. Después describe por qué el dominio es portable, por qué WinForms delega al presenter, cómo versionas JSON, cómo haces visibles archivos corruptos, por qué el diagnóstico evita PII y por qué el backup nunca repara sobre el original.

Preguntas probables:

- ¿Qué aporta `Option Strict On`?
- ¿Por qué una cotización aprobada deja de ser editable?
- ¿Cómo evitas confiar ciegamente en JSON local?
- ¿Qué diferencia hay entre build, publish e installer?
- ¿Por qué los tests del núcleo corren también en Linux si la UI es WinForms?
- ¿Qué cambiarías si los documentos vivieran en una base de datos compartida?

## Resumen

Completar el curso demuestra que puedes mantener una aplicación VB.NET pequeña con hábitos profesionales iniciales. No garantiza empleo; sí deja evidencia concreta que puedes explicar y extender.

## Referencias
- [Visual Basic documentation](https://learn.microsoft.com/dotnet/visual-basic/)
- [.NET testing](https://learn.microsoft.com/dotnet/core/testing/)
- [Windows Forms](https://learn.microsoft.com/dotnet/desktop/winforms/)
- [System.Text.Json](https://learn.microsoft.com/dotnet/standard/serialization/system-text-json/overview)

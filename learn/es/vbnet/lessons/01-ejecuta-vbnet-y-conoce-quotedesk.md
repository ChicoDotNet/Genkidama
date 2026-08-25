# Lección 01 — Ejecuta VB.NET y conoce QuoteDesk

## Qué vas a conseguir

Compilarás una aplicación VB.NET moderna y reconocerás la separación entre el núcleo portable y el host WinForms.

## El problema

Antes de aprender sintaxis necesitas ver un producto. QuoteDesk prepara cotizaciones con cliente, partidas y total.

[EJECUTAR]

```powershell
dotnet test ../app/QuoteDesk.Tests/QuoteDesk.Tests.vbproj -c Release
dotnet build ../app/QuoteDesk.WinForms/QuoteDesk.WinForms.vbproj -c Release
```

El segundo comando se ejecuta en Windows porque WinForms es una tecnología de escritorio Windows.

## Conceptos

- `.vbproj` describe un proyecto .NET.
- `net10.0` es el target portable del núcleo.
- `net10.0-windows` declara que el host usa APIs de Windows.
- `Option Strict On` evita conversiones implícitas peligrosas.

## Tu turno

Abre `QuoteLine.vb` e identifica constructor, propiedades y la propiedad calculada `LineTotal`. Cambia únicamente el fixture de una prueba y predice el total antes de ejecutarla.

## Siguiente paso

[Lección 02 — Modela una partida con tipos explícitos](02-modela-una-partida-con-tipos-explicitos.md).
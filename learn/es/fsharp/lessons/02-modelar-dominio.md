# Lección 02 — Modela el dominio con records y uniones discriminadas

## Qué vas a conseguir

Representarás clientes, partidas y cotizaciones con tipos que expresan reglas del negocio.

## El problema

Strings sueltos como `"partner"` permiten errores de escritura y estados ambiguos. Necesitamos un modelo que sólo permita categorías conocidas.

## Concepto

Una unión discriminada enumera casos válidos; un record agrupa datos con nombres claros. En [`Domain.fs`](../app/QuoteRules/Domain.fs), `CustomerTier` sólo permite `Standard`, `Preferred` o `Partner`, mientras `QuoteLine` agrupa descripción, cantidad y precio.

[EN PANTALLA]

Observa cómo el compilador conoce cada campo y cada caso antes de ejecutar el programa. Esa información será útil cuando escribamos reglas exhaustivas.

## Código real

Lee [`../app/QuoteRules/Domain.fs`](../app/QuoteRules/Domain.fs) y localiza `CustomerTier`, `QuoteLine` y `Quote`.

## Tu turno

Agrega temporalmente una partida nueva en `Program.fs` usando los mismos campos. Compila y corrige cualquier error de nombre o tipo que reporte F#.

```bash
dotnet build app/QuoteRules/QuoteRules.fsproj
```

## Buenas prácticas

Modela primero conceptos del dominio, no detalles de pantalla o almacenamiento. Prefiere estados imposibles de representar frente a validaciones dispersas.

## Resumen

Ya tienes vocabulario de dominio explícito. Ahora convertirás las reglas de descuento en funciones pequeñas y comprobables.

[Anterior](01-primera-cotizacion.md) · [Siguiente](03-reglas-puras.md)

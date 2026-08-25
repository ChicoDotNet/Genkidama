# Lección 03 — Convierte reglas de precios en funciones puras

## Qué vas a conseguir

Separarás cálculos de negocio de la entrada/salida para que sean fáciles de leer y probar.

## El problema

Si cálculo, impresión y validación viven mezclados, una regla de descuento se vuelve difícil de reutilizar y verificar.

## Concepto

Una función pura produce el mismo resultado para los mismos argumentos y no modifica estado externo. `lineSubtotal` y `discountRate` en [`Pricing.fs`](../app/QuoteRules/Pricing.fs) son ejemplos directos.

El `match` sobre `CustomerTier` obliga a expresar qué sucede para cada forma relevante del dato. Los guards `when` permiten combinar el caso con una condición del subtotal.

## Demostración

Evalúa mentalmente:

- `discountRate Preferred 499m` → `0m`;
- `discountRate Preferred 500m` → `0.05m`;
- `discountRate Partner 1000m` → `0.10m`.

Después contrasta esas expectativas ejecutando las pruebas en la lección 5.

## Tu turno

Sin cambiar la regla existente, identifica tres valores frontera que deberían protegerse con pruebas: justo debajo, justo en y por encima de un umbral.

## Buenas prácticas

Mantén dinero en `decimal`; evita `float` para cálculos monetarios ordinarios. Separa la decisión de descuento de cualquier formato de consola.

## Resumen

Las reglas puras reducen superficie accidental. Falta resolver qué hacemos cuando la entrada es inválida.

[Anterior](02-modelar-dominio.md) · [Siguiente](04-errores-result.md)

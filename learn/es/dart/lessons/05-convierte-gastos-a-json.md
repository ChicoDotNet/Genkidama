# Lección 05 — Convierte gastos a JSON sin romper el dominio

## Qué vas a conseguir

Harás que un `Expense` pueda cruzar una frontera de persistencia sin convertir el modelo en código de archivos o Flutter.

## El problema

PocketLedger ya registra gastos, pero desaparecen al cerrar la aplicación. Antes de escribir un archivo necesitamos una representación estable y validable.

## Concepto

JSON sólo conoce objetos, listas, texto, números, booleanos y `null`. Nuestro dominio conoce `ExpenseCategory`, `DateTime` y la regla de usar centavos enteros. La conversión debe ser explícita.

[DEMO]

Revisa [`../app/lib/domain/expense.dart`](../app/lib/domain/expense.dart). `toJson()` produce únicamente valores seguros para JSON y `Expense.fromJson()` vuelve a pasar por el constructor normal. Así, datos persistidos no pueden saltarse las mismas reglas que un gasto creado desde la UI.

Observa dos decisiones:

- la categoría se guarda por `name`, no por posición numérica;
- la fecha se normaliza a UTC e ISO-8601.

Si falta un campo, llega un tipo incorrecto, la categoría es desconocida o la fecha no se puede interpretar, se produce `FormatException`. No regresamos un gasto parcialmente válido.

## Tu turno

Añade una prueba que convierta un gasto a JSON y vuelva a construirlo. Después cambia `category` en el mapa por `"inventada"` y comprueba que el parseo falla.

## Errores comunes

- Persistir `double` cuando el dominio usa centavos enteros.
- Guardar `enum.index`; al reordenar el enum cambiaría el significado histórico.
- Atrapar cualquier excepción y devolver valores vacíos.
- Meter `File`, widgets o preferencias dentro de `Expense`.

## Siguiente paso

Ahora que el objeto tiene una representación portable, construiremos una frontera que realmente escriba y lea datos locales.

[Continúa con la lección 06](06-persistencia-local-versionada.md).

## Referencias

- https://dart.dev/libraries/dart-convert
- https://api.dart.dev/dart-core/DateTime/toIso8601String.html

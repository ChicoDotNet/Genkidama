# Lección 04 — Haz visibles los errores con Result

## Qué vas a conseguir

Representarás validaciones fallidas como datos explícitos en vez de ocultarlas o depender de excepciones para reglas esperables.

## El problema

Una partida con cantidad cero o precio negativo no es una condición excepcional del runtime: es una entrada inválida del dominio y debe ser observable.

## Concepto

`Result<'ok,'error>` tiene dos casos: `Ok valor` y `Error detalle`. `validateLine` devuelve uno u otro; `quote` propaga el primer error y sólo calcula cuando todas las partidas son válidas.

[DEMO]

Cambia una cantidad a `0` en `Program.fs`. Ejecuta y observa que el programa toma la rama `Error` y termina con código distinto de cero.

## Código real

Estudia `validateLine` y el `List.fold` en [`Pricing.fs`](../app/QuoteRules/Pricing.fs). El acumulador conserva o bien una lista válida o bien un error; no existe un tercer estado ambiguo.

## Errores comunes

- devolver `0m` ante datos inválidos y esconder el problema;
- lanzar excepciones para una validación normal de negocio;
- usar mensajes vagos que no permiten corregir la entrada.

## Tu turno

Prueba manualmente una descripción vacía y un precio negativo. Anota qué mensaje recibe cada caso.

## Resumen

Ahora el camino feliz y el fallo forman parte del contrato. En la siguiente lección protegerás ese contrato con pruebas automatizadas.

[Anterior](03-reglas-puras.md) · [Siguiente](05-pruebas.md)

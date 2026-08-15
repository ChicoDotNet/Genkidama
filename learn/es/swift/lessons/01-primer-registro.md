# Lección 01 — Ejecuta TimeQuote y registra tu primer trabajo

## Qué vas a conseguir

Vas a comprobar el toolchain, ejecutar una aplicación Swift real y modificar el primer registro visible de TimeQuote.

## Antes de empezar

Necesitas Swift 6.3. Compruébalo con:

```bash
swift --version
```

SwiftPM viene incluido con Swift. No necesitas instalar un package manager adicional.

## El problema

Un freelancer necesita saber cuánto tiempo trabajó para un cliente y cuánto debe cobrar. Antes de diseñar pantallas necesitamos una regla observable: **90 minutos a una tarifa determinada deben producir un importe**.

## Concepto

Un programa Swift comienza transformando datos en un resultado. En este curso evitaremos memorizar sintaxis aislada: cada concepto modifica TimeQuote.

[DEMO]

Desde `app/` ejecuta:

```bash
swift run TimeQuote
```

Observa cliente, minutos e importe.

## Código real

Abre [`../app/Sources/TimeQuote/main.swift`](../app/Sources/TimeQuote/main.swift). Verás constantes con `let`, una variable mutable con `var`, inicializadores que pueden fallar con `try` y manejo explícito mediante `do/catch`.

Cambia los minutos de `90` a `120` y vuelve a ejecutar. El importe debe cambiar sin tocar la fórmula.

## Qué acaba de pasar

- `let` protege valores que no deben reasignarse.
- `var` permite mutación intencional.
- `try` hace visible que una operación puede fallar.
- `do/catch` obliga a decidir qué hacer con ese fallo.

## Tu turno

Cambia el nombre del cliente, la nota y los minutos. Comprueba que el programa siga ejecutándose.

## Cómo comprobar tu solución

```bash
swift run TimeQuote
```

La salida debe reflejar tus datos y terminar con código 0.

## Errores comunes

- Ejecutar el comando fuera de `app/`.
- Eliminar `try` para ocultar un error de compilación.
- Cambiar varias reglas a la vez y perder la causa de un fallo.

## Buenas prácticas

Haz un cambio observable por vez y vuelve a ejecutar. Esa disciplina será más valiosa que memorizar veinte palabras reservadas.

## Resumen

Ya ejecutaste Swift, modificaste código real y observaste una regla de negocio.

## Siguiente paso

Continúa con [modelado del dominio](02-modela-el-dominio.md).

## Referencias

- https://www.swift.org/getting-started/
- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/aguideforthenewtotheshiftlanguage

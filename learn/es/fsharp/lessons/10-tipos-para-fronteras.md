# Lección 10 — Tipos para fronteras inválidas

## Qué vas a conseguir

Vas a usar un tipo de caso único para que una ruta de salida sólo exista dentro del dominio cuando cumple una regla mínima: ser una ruta no vacía con extensión `.txt`.

## El problema

Un `string` puede representar demasiadas cosas. Si una función que guarda cotizaciones recibe cualquier cadena, también acepta por accidente `""`, espacios o una extensión que el resto del programa no sabe tratar.

QuoteRules introduce `OutputFile`:

```fsharp
type OutputFile = private OutputFile of string

module OutputFile =
    let create path =
        // valida y devuelve Result<OutputFile,string>
```

El constructor real es privado. El resto de la aplicación debe pasar por `OutputFile.create`.

## Por qué importa

Esto no elimina todos los errores del filesystem: una ruta válida por forma todavía puede apuntar a una ubicación sin permisos. Sí evita que una parte de la aplicación omita la validación acordada.

Observa la diferencia entre:

```fsharp
let save (path: string) quote = ...
```

y:

```fsharp
let save (output: OutputFile) quote = ...
```

La segunda firma comunica más intención y reduce estados inválidos representables.

## Ejercicio

Agrega una regla para rechazar nombres cuyo archivo final sea sólo `.txt`. Escribe primero una prueba que falle y conserva el mensaje de error como parte del contrato observable.

## Failure modes

Distingue dos familias:

- error de dominio/configuración: ruta vacía o extensión incorrecta;
- error operativo: permisos, disco o ruta inaccesible al guardar.

No conviertas ambos en una excepción genérica invisible.

## Referencias oficiales

- [F# discriminated unions](https://learn.microsoft.com/dotnet/fsharp/language-reference/discriminated-unions)
- [F# access control](https://learn.microsoft.com/dotnet/fsharp/language-reference/access-control)

[Anterior](09-checkpoint-entrada.md) · [Siguiente](11-reportes-deterministas.md)

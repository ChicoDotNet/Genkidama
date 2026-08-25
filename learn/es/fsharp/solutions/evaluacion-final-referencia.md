# Solución de referencia — Evaluación final de QuoteRules

Esta es una solución posible, no una plantilla obligatoria. Compárala con tu implementación sólo después de haber intentado resolver el cambio.

## 1. Modela la referencia cerca del dominio

Una opción razonable es crear un tipo que sólo pueda construirse mediante una función de validación:

```fsharp
type QuoteReference = private QuoteReference of string

module QuoteReference =
    let create raw =
        let value = raw.Trim()

        if System.String.IsNullOrWhiteSpace value then
            Error "La referencia no puede estar vacía."
        elif value.Length > 40 then
            Error "La referencia no puede exceder 40 caracteres."
        else
            Ok (QuoteReference value)

    let value (QuoteReference value) = value
```

El límite de 40 caracteres es una decisión de esta referencia. Si elegiste otro contrato razonable y lo documentaste, no es automáticamente incorrecto.

## 2. Haz opcional la presencia, no la validez

Modela la ausencia con `option`:

```fsharp
QuoteReference option
```

Así existen dos estados claros: no se proporcionó referencia, o existe una referencia ya validada. Evita usar `""` como tercer estado implícito.

## 3. Mantén Pricing ajeno al dato

La referencia identifica la cotización; no modifica subtotal, descuento ni total. Por eso `Pricing` no necesita conocerla. Si tu solución cambió funciones monetarias sólo para transportar texto, revisa la separación de responsabilidades.

## 4. Valida en la frontera de entrada

Cuando el usuario proporcione la referencia, conviértela a `QuoteReference` antes de construir el dato que llegará a reporting. Un error debe propagarse como `Error` y detener la persistencia.

Una forma posible:

```fsharp
let parseOptionalReference raw =
    match raw with
    | None -> Ok None
    | Some value -> QuoteReference.create value |> Result.map Some
```

## 5. Extiende el reporte sin perder determinismo

`Reporting.render` puede recibir la referencia junto con el resultado calculado y añadir una línea sólo cuando exista:

```fsharp
match reference with
| Some value -> $"Referencia: {QuoteReference.value value}"
| None -> ""
```

La forma exacta del string puede variar. Lo importante es que la misma entrada produzca la misma salida y que el rendering siga separado de `File.WriteAllText`.

## 6. Pruebas valiosas

Como mínimo protege:

- una referencia válida aparece en el reporte;
- una referencia vacía es rechazada;
- una ejecución sin referencia conserva el comportamiento previo;
- si modificaste parsing de CLI, una entrada inválida devuelve un resultado observable de error.

Evita pruebas que sólo comprueben getters triviales o reproduzcan línea por línea la implementación.

## 7. Bugfix controlado

Un ejercicio seguro consiste en invertir temporalmente la condición que rechaza referencias vacías. Primero confirma que tu prueba falla; luego restaura la condición y vuelve a ejecutar la suite. Esa evidencia muestra que la prueba protege realmente el contrato.

## 8. Diseño posterior

Una mejora razonable sería admitir JSON además de texto. El cambio debería introducir otra representación de salida alrededor de `Reporting`, no dentro de `Pricing`. Si el número de formatos creciera, entonces sí valdría la pena revisar la abstracción de salida, pero no antes de que el problema exista.

## Comprobación

```bash
dotnet build app/QuoteRules/QuoteRules.fsproj --configuration Release
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --configuration Release
```

Vuelve a la [rúbrica](../lessons/16-rubrica-y-solucion.md) y califica tu solución por contratos y evidencia, no por parecido textual con esta referencia.

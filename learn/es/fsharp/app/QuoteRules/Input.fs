namespace QuoteRules

open System
open System.Globalization

module Input =
    let parseTier (value: string) =
        match value.Trim().ToLowerInvariant() with
        | "standard" -> Ok Standard
        | "preferred" -> Ok Preferred
        | "partner" -> Ok Partner
        | _ -> Error $"Nivel de cliente desconocido: {value}"

    let parseLine (value: string) =
        let parts = value.Split('|', StringSplitOptions.TrimEntries)

        if parts.Length <> 3 then
            Error "Cada partida debe usar el formato descripcion|cantidad|precio."
        else
            match Int32.TryParse parts[1], Decimal.TryParse(parts[2], NumberStyles.Number, CultureInfo.InvariantCulture) with
            | (true, quantity), (true, unitPrice) ->
                Pricing.validateLine
                    { Description = parts[0]
                      Quantity = quantity
                      UnitPrice = unitPrice }
            | (false, _), _ -> Error $"Cantidad inválida: {parts[1]}"
            | _, (false, _) -> Error $"Precio inválido: {parts[2]}"

    let parseLines (values: string seq) =
        values
        |> Seq.fold
            (fun state value ->
                match state, parseLine value with
                | Error error, _ -> Error error
                | _, Error error -> Error error
                | Ok lines, Ok line -> Ok(line :: lines))
            (Ok [])
        |> Result.map List.rev

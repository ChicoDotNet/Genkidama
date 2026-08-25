namespace QuoteRules

module Pricing =
    let validateLine line =
        if System.String.IsNullOrWhiteSpace line.Description then
            Error "La descripción es obligatoria."
        elif line.Quantity <= 0 then
            Error "La cantidad debe ser mayor que cero."
        elif line.UnitPrice < 0m then
            Error "El precio unitario no puede ser negativo."
        else
            Ok line

    let lineSubtotal line =
        decimal line.Quantity * line.UnitPrice

    let discountRate tier subtotal =
        match tier with
        | Partner when subtotal >= 1000m -> 0.10m
        | Preferred when subtotal >= 500m -> 0.05m
        | _ -> 0m

    let quote tier lines =
        let folder state line =
            match state, validateLine line with
            | Error error, _ -> Error error
            | _, Error error -> Error error
            | Ok validLines, Ok validLine -> Ok(validLine :: validLines)

        match List.fold folder (Ok []) lines with
        | Error error -> Error error
        | Ok validLines ->
            let orderedLines = List.rev validLines
            let subtotal = orderedLines |> List.sumBy lineSubtotal
            let rate = discountRate tier subtotal
            let discount = subtotal * rate

            Ok
                { Lines = orderedLines
                  Subtotal = subtotal
                  DiscountRate = rate
                  Discount = discount
                  Total = subtotal - discount }

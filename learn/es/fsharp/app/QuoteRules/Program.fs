open QuoteRules

let lines =
    [ { Description = "Consultoría"
        Quantity = 2
        UnitPrice = 350m }
      { Description = "Implementación"
        Quantity = 1
        UnitPrice = 600m } ]

match Pricing.quote Preferred lines with
| Error error ->
    eprintfn "No se pudo cotizar: %s" error
    1
| Ok quote ->
    printfn "Subtotal: %M" quote.Subtotal
    printfn "Descuento: %M" quote.Discount
    printfn "Total: %M" quote.Total
    0
|> exit

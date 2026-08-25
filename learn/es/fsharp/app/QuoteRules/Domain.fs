namespace QuoteRules

open System

type CustomerTier =
    | Standard
    | Preferred
    | Partner

type QuoteLine =
    { Description: string
      Quantity: int
      UnitPrice: decimal }

type Quote =
    { Lines: QuoteLine list
      Subtotal: decimal
      DiscountRate: decimal
      Discount: decimal
      Total: decimal }

type OutputFile = private OutputFile of string

module OutputFile =
    let create path =
        if String.IsNullOrWhiteSpace path then
            Error "La ruta de salida es obligatoria."
        elif not (path.EndsWith(".txt", StringComparison.OrdinalIgnoreCase)) then
            Error "La salida debe usar extensión .txt."
        else
            Ok(OutputFile path)

    let value (OutputFile path) = path

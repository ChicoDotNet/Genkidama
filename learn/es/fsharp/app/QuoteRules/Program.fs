open System
open System.IO
open QuoteRules

let sampleLines =
    [ "Consultoría|2|350"
      "Implementación|1|600" ]

let args = Environment.GetCommandLineArgs() |> Array.skip 1

let tierResult, lineSource =
    match args with
    | [| tier; path |] when File.Exists path ->
        Input.parseTier tier, File.ReadLines path
    | [| _; path |] ->
        Error $"No existe el archivo: {path}", Seq.empty
    | [| tier |] ->
        Input.parseTier tier, sampleLines
    | _ ->
        Ok Preferred, sampleLines

match tierResult with
| Error error ->
    eprintfn "No se pudo cotizar: %s" error
    1
| Ok tier ->
    match Input.parseLines lineSource with
    | Error error ->
        eprintfn "No se pudo cotizar: %s" error
        1
    | Ok lines ->
        match Pricing.quote tier lines with
        | Error error ->
            eprintfn "No se pudo cotizar: %s" error
            1
        | Ok quote ->
            printfn "Subtotal: %M" quote.Subtotal
            printfn "Descuento: %M" quote.Discount
            printfn "Total: %M" quote.Total
            0
|> exit

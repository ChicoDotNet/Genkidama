open System
open System.IO
open QuoteRules

let sampleLines =
    [ "Consultoría|2|350"
      "Implementación|1|600" ]

let args = Environment.GetCommandLineArgs() |> Array.skip 1

let tierResult, lineSource, outputResult =
    match args with
    | [| tier; path; output |] when File.Exists path ->
        Input.parseTier tier, File.ReadLines path, OutputFile.create output |> Some
    | [| _; path; _ |] ->
        Error $"No existe el archivo: {path}", Seq.empty, None
    | [| tier; path |] when File.Exists path ->
        Input.parseTier tier, File.ReadLines path, None
    | [| _; path |] ->
        Error $"No existe el archivo: {path}", Seq.empty, None
    | [| tier |] ->
        Input.parseTier tier, sampleLines, None
    | _ ->
        Ok Preferred, sampleLines, None

let printQuote quote =
    printfn "Subtotal: %M" quote.Subtotal
    printfn "Descuento: %M" quote.Discount
    printfn "Total: %M" quote.Total

    match outputResult with
    | None -> Ok()
    | Some(Error error) -> Error error
    | Some(Ok output) ->
        match Reporting.save output quote with
        | Error error -> Error error
        | Ok path ->
            printfn "Cotización guardada en: %s" path
            Ok()

let result =
    tierResult
    |> Result.bind (fun tier -> Input.parseLines lineSource |> Result.bind (Pricing.quote tier))
    |> Result.bind (fun quote ->
        printQuote quote
        |> Result.map (fun () -> quote))

match result with
| Error error ->
    eprintfn "No se pudo cotizar: %s" error
    1
| Ok _ -> 0
|> exit

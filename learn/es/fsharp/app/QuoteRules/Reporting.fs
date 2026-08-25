namespace QuoteRules

open System
open System.Globalization
open System.IO

module Reporting =
    let private money (value: decimal) = value.ToString("0.00", CultureInfo.InvariantCulture)

    let render quote =
        [ yield $"Subtotal={money quote.Subtotal}"
          yield $"DiscountRate={money quote.DiscountRate}"
          yield $"Discount={money quote.Discount}"
          yield $"Total={money quote.Total}"
          yield "Lines:"

          for line in quote.Lines do
              yield $"- {line.Description}|{line.Quantity}|{money line.UnitPrice}" ]
        |> String.concat Environment.NewLine

    let save output quote =
        try
            let path = output |> OutputFile.value |> Path.GetFullPath
            let directory = Path.GetDirectoryName path

            if not (String.IsNullOrWhiteSpace directory) then
                Directory.CreateDirectory directory |> ignore

            File.WriteAllText(path, render quote)
            Ok path
        with ex ->
            Error $"No se pudo guardar la cotización: {ex.Message}"

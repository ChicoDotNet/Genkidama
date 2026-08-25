namespace QuoteRules.Tests

open System.IO
open QuoteRules
open Xunit

module PricingTests =
    [<Fact>]
    let ``valid line calculates subtotal`` () =
        let line = { Description = "Servicio"; Quantity = 3; UnitPrice = 125m }
        Assert.Equal(375m, Pricing.lineSubtotal line)

    [<Fact>]
    let ``preferred customer receives five percent above threshold`` () =
        Assert.Equal(0.05m, Pricing.discountRate Preferred 500m)

    [<Fact>]
    let ``standard customer does not receive tier discount`` () =
        Assert.Equal(0m, Pricing.discountRate Standard 5000m)

    [<Fact>]
    let ``invalid quantity is rejected explicitly`` () =
        let line = { Description = "Servicio"; Quantity = 0; UnitPrice = 10m }
        Assert.Equal(Error "La cantidad debe ser mayor que cero.", Pricing.validateLine line)

    [<Fact>]
    let ``quote combines subtotal discount and total`` () =
        let lines =
            [ { Description = "A"; Quantity = 2; UnitPrice = 300m }
              { Description = "B"; Quantity = 1; UnitPrice = 400m } ]

        match Pricing.quote Partner lines with
        | Error error -> failwith error
        | Ok quote ->
            Assert.Equal(1000m, quote.Subtotal)
            Assert.Equal(0.10m, quote.DiscountRate)
            Assert.Equal(100m, quote.Discount)
            Assert.Equal(900m, quote.Total)

module InputTests =
    [<Theory>]
    [<InlineData("standard")>]
    [<InlineData(" Standard ")>]
    let ``standard tier parsing is case and whitespace tolerant`` value =
        Assert.Equal(Ok Standard, Input.parseTier value)

    [<Fact>]
    let ``unknown tier is rejected`` () =
        match Input.parseTier "vip" with
        | Error error -> Assert.Contains("desconocido", error)
        | Ok _ -> failwith "Expected invalid tier to fail"

    [<Fact>]
    let ``line parser converts external text into typed domain data`` () =
        let expected = { Description = "Consultoría"; Quantity = 2; UnitPrice = 350m }
        Assert.Equal(Ok expected, Input.parseLine "Consultoría|2|350")

    [<Fact>]
    let ``line parser rejects malformed quantity`` () =
        Assert.Equal(Error "Cantidad inválida: dos", Input.parseLine "Consultoría|dos|350")

    [<Fact>]
    let ``parseLines preserves source order`` () =
        match Input.parseLines [ "A|1|10"; "B|2|20" ] with
        | Error error -> failwith error
        | Ok lines -> Assert.Equal<string list>([ "A"; "B" ], lines |> List.map _.Description)

module ReportingTests =
    let private sampleQuote =
        { Lines = [ { Description = "Servicio"; Quantity = 2; UnitPrice = 125m } ]
          Subtotal = 250m
          DiscountRate = 0.05m
          Discount = 12.5m
          Total = 237.5m }

    [<Fact>]
    let ``output file rejects non text extension`` () =
        Assert.Equal(Error "La salida debe usar extensión .txt.", OutputFile.create "quote.json")

    [<Fact>]
    let ``report is deterministic and culture independent`` () =
        let rendered = Reporting.render sampleQuote
        Assert.Contains("Subtotal=250.00", rendered)
        Assert.Contains("DiscountRate=0.05", rendered)
        Assert.Contains("- Servicio|2|125.00", rendered)

    [<Fact>]
    let ``saving report creates parent directory`` () =
        let root = Path.Combine(Path.GetTempPath(), $"quoterules-{System.Guid.NewGuid():N}")
        let path = Path.Combine(root, "nested", "quote.txt")

        try
            match OutputFile.create path with
            | Error error -> failwith error
            | Ok output ->
                match Reporting.save output sampleQuote with
                | Error error -> failwith error
                | Ok savedPath ->
                    Assert.True(File.Exists savedPath)
                    Assert.Contains("Total=237.50", File.ReadAllText savedPath)
        finally
            if Directory.Exists root then
                Directory.Delete(root, true)

type IReport =
    abstract member Generate: unit -> unit

type PDFReport() =
    interface IReport with
        member _.Generate() = printfn "Generating PDF report"

type HTMLReport() =
    interface IReport with
        member _.Generate() = printfn "Generating HTML report"

type IReportFactory =
    abstract member CreateReport: unit -> IReport

type PDFReportFactory() =
    interface IReportFactory with
        member _.CreateReport() = PDFReport() :> IReport

type HTMLReportFactory() =
    interface IReportFactory with
        member _.CreateReport() = HTMLReport() :> IReport

let useFactory (factory: IReportFactory) =
    let report = factory.CreateReport()
    report.Generate()

// Usage
useFactory (PDFReportFactory() :> IReportFactory)
useFactory (HTMLReportFactory() :> IReportFactory)

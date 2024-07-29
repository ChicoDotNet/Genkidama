type
  Report = ref object of RootObj
  proc generate(self: Report)

type
  PDFReport = ref object of Report
  HTMLReport = ref object of Report

proc generate(self: PDFReport) =
  echo "Generating PDF report"

proc generate(self: HTMLReport) =
  echo "Generating HTML report"

type
  ReportFactory = ref object of RootObj
  proc createReport(self: ReportFactory): Report

type
  PDFReportFactory = ref object of ReportFactory
  HTMLReportFactory = ref object of ReportFactory

proc createReport(self: PDFReportFactory): Report =
  PDFReport()

proc createReport(self: HTMLReportFactory): Report =
  HTMLReport()

proc useFactory(factory: ReportFactory) =
  let report = factory.createReport()
  report.generate()

# Usage
let pdfFactory = PDFReportFactory()
let htmlFactory = HTMLReportFactory()
useFactory(pdfFactory)
useFactory(htmlFactory)

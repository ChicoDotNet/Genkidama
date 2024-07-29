class Report r where
  generate :: r -> IO ()

data PDFReport = PDFReport
data HTMLReport = HTMLReport

instance Report PDFReport where
  generate _ = putStrLn "Generating PDF report"

instance Report HTMLReport where
  generate _ = putStrLn "Generating HTML report"

class ReportFactory rf where
  createReport :: rf -> IO ()

data PDFReportFactory = PDFReportFactory
data HTMLReportFactory = HTMLReportFactory

instance ReportFactory PDFReportFactory where
  createReport _ = do
    let report = PDFReport
    generate report

instance ReportFactory HTMLReportFactory where
  createReport _ = do
    let report = HTMLReport
    generate report

useFactory :: (ReportFactory rf) => rf -> IO ()
useFactory factory = createReport factory

main :: IO ()
main = do
  useFactory PDFReportFactory
  useFactory HTMLReportFactory

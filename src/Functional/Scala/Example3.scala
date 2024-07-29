trait Report {
  def generate(): Unit
}

class PDFReport extends Report {
  def generate(): Unit = println("Generating PDF report")
}

class HTMLReport extends Report {
  def generate(): Unit = println("Generating HTML report")
}

trait ReportFactory {
  def createReport(): Report
}

class PDFReportFactory extends ReportFactory {
  def createReport(): Report = new PDFReport
}

class HTMLReportFactory extends ReportFactory {
  def createReport(): Report = new HTMLReport
}

object Example3 {
  def useFactory(factory: ReportFactory): Unit = {
    val report = factory.createReport()
    report.generate()
  }

  def main(args: Array[String]): Unit = {
    useFactory(new PDFReportFactory)
    useFactory(new HTMLReportFactory)
  }
}

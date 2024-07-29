# Abstract Product
abstract class Report
  abstract def generate : Void
end

# Concrete Product
class PDFReport < Report
  def generate : Void
    puts "Generating PDF report"
  end
end

class HTMLReport < Report
  def generate : Void
    puts "Generating HTML report"
  end
end

# Abstract Factory
abstract class ReportFactory
  abstract def create_report : Report
end

# Concrete Factory
class PDFReportFactory < ReportFactory
  def create_report : Report
    PDFReport.new
  end
end

class HTMLReportFactory < ReportFactory
  def create_report : Report
    HTMLReport.new
  end
end

def use_factory(factory : ReportFactory) : Void
  report = factory.create_report
  report.generate
end

# Usage
use_factory PDFReportFactory.new
use_factory HTMLReportFactory.new

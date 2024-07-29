# Abstract Factory
class ReportFactory
    def create_report
      raise NotImplementedError, 'create_report() must be implemented'
    end
  end
  
  # Concrete Factories
  class PDFReportFactory < ReportFactory
    def create_report
      PDFReport.new
    end
  end
  
  class HTMLReportFactory < ReportFactory
    def create_report
      HTMLReport.new
    end
  end
  
  # Products
  class PDFReport
    def generate
      puts 'Generating PDF report'
    end
  end
  
  class HTMLReport
    def generate
      puts 'Generating HTML report'
    end
  end
  
  def use_factory(factory)
    report = factory.create_report
    report.generate
  end
  
  # Usage
  use_factory(PDFReportFactory.new)
  use_factory(HTMLReportFactory.new)
  
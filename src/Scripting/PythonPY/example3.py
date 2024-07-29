class Report:
    def generate(self):
        pass

class PDFReport(Report):
    def generate(self):
        print("Generating PDF report")

class HTMLReport(Report):
    def generate(self):
        print("Generating HTML report")

class ReportFactory:
    def create_report(self):
        pass

class PDFReportFactory(ReportFactory):
    def create_report(self):
        return PDFReport()

class HTMLReportFactory(ReportFactory):
    def create_report(self):
        return HTMLReport()

def use_factory(factory):
    report = factory.create_report()
    report.generate()

# Usage
use_factory(PDFReportFactory())
use_factory(HTMLReportFactory())

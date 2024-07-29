// Abstract Product
abstract class Report {
  void generate();
}

// Concrete Product
class PDFReport implements Report {
  @override
  void generate() {
    print('Generating PDF report');
  }
}

class HTMLReport implements Report {
  @override
  void generate() {
    print('Generating HTML report');
  }
}

// Abstract Factory
abstract class ReportFactory {
  Report createReport();
}

// Concrete Factory
class PDFReportFactory implements ReportFactory {
  @override
  Report createReport() {
    return PDFReport();
  }
}

class HTMLReportFactory implements ReportFactory {
  @override
  Report createReport() {
    return HTMLReport();
  }
}

void useFactory(ReportFactory factory) {
  var report = factory.createReport();
  report.generate();
}

// Usage
void main() {
  useFactory(PDFReportFactory());
  useFactory(HTMLReportFactory());
}

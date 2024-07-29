interface Report {
    void generate()
}

class PDFReport implements Report {
    void generate() {
        println "Generating PDF report"
    }
}

class HTMLReport implements Report {
    void generate() {
        println "Generating HTML report"
    }
}

interface ReportFactory {
    Report createReport()
}

class PDFReportFactory implements ReportFactory {
    Report createReport() {
        return new PDFReport()
    }
}

class HTMLReportFactory implements ReportFactory {
    Report createReport() {
        return new HTMLReport()
    }
}

def useFactory(ReportFactory factory) {
    def report = factory.createReport()
    report.generate()
}

// Usage
useFactory(new PDFReportFactory())
useFactory(new HTMLReportFactory())

import Foundation

// Abstract Product
protocol Report {
    func generate()
}

// Concrete Product
class PDFReport: Report {
    func generate() {
        print("Generating PDF report")
    }
}

class HTMLReport: Report {
    func generate() {
        print("Generating HTML report")
    }
}

// Abstract Factory
protocol ReportFactory {
    func createReport() -> Report
}

// Concrete Factory
class PDFReportFactory: ReportFactory {
    func createReport() -> Report {
        return PDFReport()
    }
}

class HTMLReportFactory: ReportFactory {
    func createReport() -> Report {
        return HTMLReport()
    }
}

func useFactory(factory: ReportFactory) {
    let report = factory.createReport()
    report.generate()
}

// Usage
useFactory(factory: PDFReportFactory())
useFactory(factory: HTMLReportFactory())

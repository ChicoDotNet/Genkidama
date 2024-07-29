// Abstract Product
interface Report {
    generate(): void;
}

// Concrete Product
class PDFReport implements Report {
    generate(): void {
        console.log("Generating PDF report");
    }
}

class HTMLReport implements Report {
    generate(): void {
        console.log("Generating HTML report");
    }
}

// Abstract Factory
interface ReportFactory {
    createReport(): Report;
}

// Concrete Factory
class PDFReportFactory implements ReportFactory {
    createReport(): Report {
        return new PDFReport();
    }
}

class HTMLReportFactory implements ReportFactory {
    createReport(): Report {
        return new HTMLReport();
    }
}

function useFactory(factory: ReportFactory): void {
    const report = factory.createReport();
    report.generate();
}

// Usage
useFactory(new PDFReportFactory());
useFactory(new HTMLReportFactory());

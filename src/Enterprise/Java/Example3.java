// Abstract Product
interface Report {
    void generate();
}

// Concrete Product
class PDFReport implements Report {
    @Override
    public void generate() {
        System.out.println("Generating PDF report");
    }
}

class HTMLReport implements Report {
    @Override
    public void generate() {
        System.out.println("Generating HTML report");
    }
}

// Abstract Factory
interface ReportFactory {
    Report createReport();
}

// Concrete Factory
class PDFReportFactory implements ReportFactory {
    @Override
    public Report createReport() {
        return new PDFReport();
    }
}

class HTMLReportFactory implements ReportFactory {
    @Override
    public Report createReport() {
        return new HTMLReport();
    }
}

public class Example3 {
    public static void useFactory(ReportFactory factory) {
        Report report = factory.createReport();
        report.generate();
    }

    public static void main(String[] args) {
        useFactory(new PDFReportFactory());
        useFactory(new HTMLReportFactory());
    }
}

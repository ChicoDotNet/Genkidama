using System;

// Abstract Product
public interface Report {
    void Generate();
}

// Concrete Product
public class PDFReport : Report {
    public void Generate() {
        Console.WriteLine("Generating PDF report");
    }
}

public class HTMLReport : Report {
    public void Generate() {
        Console.WriteLine("Generating HTML report");
    }
}

// Abstract Factory
public interface ReportFactory {
    Report CreateReport();
}

// Concrete Factory
public class PDFReportFactory : ReportFactory {
    public Report CreateReport() {
        return new PDFReport();
    }
}

public class HTMLReportFactory : ReportFactory {
    public Report CreateReport() {
        return new HTMLReport();
    }
}

public class Example3 {
    public static void UseFactory(ReportFactory factory) {
        var report = factory.CreateReport();
        report.Generate();
    }

    public static void Main(string[] args) {
        UseFactory(new PDFReportFactory());
        UseFactory(new HTMLReportFactory());
    }
}

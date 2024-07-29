// Abstract Factory
function createReport(reportType) {
    if (reportType === "pdf") {
        return new PDFReport();
    } else if (reportType === "html") {
        return new HTMLReport();
    }
}

// Concrete Products
class PDFReport {
    generate() {
        console.log("Generating PDF report");
    }
}

class HTMLReport {
    generate() {
        console.log("Generating HTML report");
    }
}

// Usage
const report1 = createReport("pdf");
report1.generate();

const report2 = createReport("html");
report2.generate();

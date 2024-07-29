trait Report {
    fn generate(&self);
}

struct PDFReport;

impl Report for PDFReport {
    fn generate(&self) {
        println!("Generating PDF report");
    }
}

struct HTMLReport;

impl Report for HTMLReport {
    fn generate(&self) {
        println!("Generating HTML report");
    }
}

trait ReportFactory {
    fn create_report(&self) -> Box<dyn Report>;
}

struct PDFReportFactory;

impl ReportFactory for PDFReportFactory {
    fn create_report(&self) -> Box<dyn Report> {
        Box::new(PDFReport)
    }
}

struct HTMLReportFactory;

impl ReportFactory for HTMLReportFactory {
    fn create_report(&self) -> Box<dyn Report> {
        Box::new(HTMLReport)
    }
}

fn use_factory(factory: &dyn ReportFactory) {
    let report = factory.create_report();
    report.generate();
}

fn main() {
    let pdf_factory = PDFReportFactory;
    let html_factory = HTMLReportFactory;
    use_factory(&pdf_factory);
    use_factory(&html_factory);
}

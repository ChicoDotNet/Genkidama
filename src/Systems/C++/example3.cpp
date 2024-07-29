#include <iostream>

// Abstract Product
class Report {
public:
    virtual void generate() = 0;
};

// Concrete Product
class PDFReport : public Report {
public:
    void generate() override {
        std::cout << "Generating PDF report" << std::endl;
    }
};

class HTMLReport : public Report {
public:
    void generate() override {
        std::cout << "Generating HTML report" << std::endl;
    }
};

// Abstract Factory
class ReportFactory {
public:
    virtual Report* createReport() = 0;
};

// Concrete Factory
class PDFReportFactory : public ReportFactory {
public:
    Report* createReport() override {
        return new PDFReport();
    }
};

class HTMLReportFactory : public ReportFactory {
public:
    Report* createReport() override {
        return new HTMLReport();
    }
};

// Usage
void useFactory(ReportFactory* factory) {
    Report* report = factory->createReport();
    report->generate();
    delete report;
}

int main() {
    ReportFactory* pdfFactory = new PDFReportFactory();
    ReportFactory* htmlFactory = new HTMLReportFactory();
    useFactory(pdfFactory);
    useFactory(htmlFactory);
    delete pdfFactory;
    delete htmlFactory;
    return 0;
}

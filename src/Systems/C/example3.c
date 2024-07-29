#include <stdio.h>

// Abstract Factory
typedef struct {
    void (*generate)();
} Report;

// Concrete Implementations
void generate_pdf_report() {
    printf("Generating PDF report\n");
}

void generate_html_report() {
    printf("Generating HTML report\n");
}

// Concrete Factories
Report create_pdf_report_factory() {
    Report report;
    report.generate = generate_pdf_report;
    return report;
}

Report create_html_report_factory() {
    Report report;
    report.generate = generate_html_report;
    return report;
}

// Usage
void use_factory(Report report) {
    report.generate();
}

int main() {
    Report pdf_report = create_pdf_report_factory();
    Report html_report = create_html_report_factory();

    use_factory(pdf_report);
    use_factory(html_report);

    return 0;
}

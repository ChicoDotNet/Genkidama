package main

import "fmt"

// Abstract Product
type Report interface {
    Generate()
}

// Concrete Product
type PDFReport struct{}

func (p *PDFReport) Generate() {
    fmt.Println("Generating PDF report")
}

type HTMLReport struct{}

func (h *HTMLReport) Generate() {
    fmt.Println("Generating HTML report")
}

// Abstract Factory
type ReportFactory interface {
    CreateReport() Report
}

// Concrete Factory
type PDFReportFactory struct{}

func (p *PDFReportFactory) CreateReport() Report {
    return &PDFReport{}
}

type HTMLReportFactory struct{}

func (h *HTMLReportFactory) CreateReport() Report {
    return &HTMLReport{}
}

func useFactory(factory ReportFactory) {
    report := factory.CreateReport()
    report.Generate()
}

func main() {
    pdfFactory := &PDFReportFactory{}
    htmlFactory := &HTMLReportFactory{}
    useFactory(pdfFactory)
    useFactory(htmlFactory)
}

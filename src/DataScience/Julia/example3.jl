abstract type Report end

struct PDFReport <: Report end
struct HTMLReport <: Report end

function generate(r::PDFReport)
    println("Generating PDF report")
end

function generate(r::HTMLReport)
    println("Generating HTML report")
end

abstract type ReportFactory end

struct PDFReportFactory <: ReportFactory end
struct HTMLReportFactory <: ReportFactory end

function create_report(::PDFReportFactory)::Report
    return PDFReport()
end

function create_report(::HTMLReportFactory)::Report
    return HTMLReport()
end

function use_factory(factory::ReportFactory)
    report = create_report(factory)
    generate(report)
end

# Usage
use_factory(PDFReportFactory())
use_factory(HTMLReportFactory())

extends Node

# Abstract Factory
func create_report(report_type: String) -> Object:
    if report_type == "pdf":
        return PDFReport.new()
    elif report_type == "html":
        return HTMLReport.new()

# Concrete Products
class PDFReport:
    func generate():
        print("Generating PDF report")
    
class HTMLReport:
    func generate():
        print("Generating HTML report")

# Usage
func _ready():
    var report1 = create_report("pdf")
    report1.generate()
    
    var report2 = create_report("html")
    report2.generate()

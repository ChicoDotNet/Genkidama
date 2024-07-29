// Abstract Product
interface Report {
    fun generate()
}

// Concrete Product
class PDFReport : Report {
    override fun generate() {
        println("Generating PDF report")
    }
}

class HTMLReport : Report {
    override fun generate() {
        println("Generating HTML report")
    }
}

// Abstract Factory
interface ReportFactory {
    fun createReport(): Report
}

// Concrete Factory
class PDFReportFactory : ReportFactory {
    override fun createReport(): Report {
        return PDFReport()
    }
}

class HTMLReportFactory : ReportFactory {
    override fun createReport(): Report {
        return HTMLReport()
    }
}

fun useFactory(factory: ReportFactory) {
    val report = factory.createReport()
    report.generate()
}

// Usage
fun main() {
    useFactory(PDFReportFactory())
    useFactory(HTMLReportFactory())
}

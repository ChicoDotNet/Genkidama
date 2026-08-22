interface ReportBuilder {
    fun reset()
    fun addTitle(title: String)
    fun addSection(heading: String, body: String)
    fun build(): String
}

class TextReportBuilder : ReportBuilder {
    private val parts = mutableListOf<String>()
    override fun reset() = parts.clear()
    override fun addTitle(title: String) { parts += "# $title" }
    override fun addSection(heading: String, body: String) {
        parts += "## $heading"
        parts += body
    }
    override fun build(): String = parts.joinToString("\n")
}

class HtmlReportBuilder : ReportBuilder {
    private val parts = mutableListOf<String>()
    override fun reset() = parts.clear()
    override fun addTitle(title: String) { parts += "<h1>$title</h1>" }
    override fun addSection(heading: String, body: String) {
        parts += "<h2>$heading</h2>"
        parts += "<p>$body</p>"
    }
    override fun build(): String = parts.joinToString("")
}

fun buildAvailabilityReport(builder: ReportBuilder): String {
    builder.reset()
    builder.addTitle("Service status")
    builder.addSection("Availability", "99.95%")
    return builder.build()
}

fun main() {
    println(buildAvailabilityReport(TextReportBuilder()))
    println("---")
    println(buildAvailabilityReport(HtmlReportBuilder()))
}

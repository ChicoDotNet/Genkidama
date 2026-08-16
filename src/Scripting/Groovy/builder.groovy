interface ReportBuilder {
    void reset()
    void addTitle(String title)
    void addSection(String heading, String body)
    String build()
}

class TextReportBuilder implements ReportBuilder {
    private final List<String> parts = []
    void reset() { parts.clear() }
    void addTitle(String title) { parts << "# $title" }
    void addSection(String heading, String body) { parts.addAll(["## $heading", body]) }
    String build() { parts.join('\n') }
}

class HtmlReportBuilder implements ReportBuilder {
    private final List<String> parts = []
    void reset() { parts.clear() }
    void addTitle(String title) { parts << "<h1>$title</h1>" }
    void addSection(String heading, String body) { parts.addAll(["<h2>$heading</h2>", "<p>$body</p>"]) }
    String build() { parts.join('') }
}

String buildAvailabilityReport(ReportBuilder builder) {
    builder.reset()
    builder.addTitle('Service status')
    builder.addSection('Availability', '99.95%')
    builder.build()
}

println buildAvailabilityReport(new TextReportBuilder())
println '---'
println buildAvailabilityReport(new HtmlReportBuilder())

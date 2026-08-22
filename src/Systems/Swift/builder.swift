protocol ReportBuilder: AnyObject {
    func reset()
    func addTitle(_ title: String)
    func addSection(_ heading: String, _ body: String)
    func build() -> String
}

final class TextReportBuilder: ReportBuilder {
    private var parts: [String] = []
    func reset() { parts.removeAll() }
    func addTitle(_ title: String) { parts.append("# \(title)") }
    func addSection(_ heading: String, _ body: String) {
        parts.append("## \(heading)")
        parts.append(body)
    }
    func build() -> String { parts.joined(separator: "\n") }
}

final class HtmlReportBuilder: ReportBuilder {
    private var parts: [String] = []
    func reset() { parts.removeAll() }
    func addTitle(_ title: String) { parts.append("<h1>\(title)</h1>") }
    func addSection(_ heading: String, _ body: String) {
        parts.append("<h2>\(heading)</h2>")
        parts.append("<p>\(body)</p>")
    }
    func build() -> String { parts.joined() }
}

func buildAvailabilityReport(_ builder: ReportBuilder) -> String {
    builder.reset()
    builder.addTitle("Service status")
    builder.addSection("Availability", "99.95%")
    return builder.build()
}

print(buildAvailabilityReport(TextReportBuilder()))
print("---")
print(buildAvailabilityReport(HtmlReportBuilder()))

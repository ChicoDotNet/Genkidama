import java.util.ArrayList;
import java.util.List;

interface ReportBuilder {
    void reset();
    void addTitle(String title);
    void addSection(String heading, String body);
    String build();
}

final class TextReportBuilder implements ReportBuilder {
    private final List<String> parts = new ArrayList<>();
    public void reset() { parts.clear(); }
    public void addTitle(String title) { parts.add("# " + title); }
    public void addSection(String heading, String body) {
        parts.add("## " + heading);
        parts.add(body);
    }
    public String build() { return String.join("\n", parts); }
}

final class HtmlReportBuilder implements ReportBuilder {
    private final List<String> parts = new ArrayList<>();
    public void reset() { parts.clear(); }
    public void addTitle(String title) { parts.add("<h1>" + title + "</h1>"); }
    public void addSection(String heading, String body) {
        parts.add("<h2>" + heading + "</h2>");
        parts.add("<p>" + body + "</p>");
    }
    public String build() { return String.join("", parts); }
}

public final class BuilderExample {
    private static String buildAvailabilityReport(ReportBuilder builder) {
        builder.reset();
        builder.addTitle("Service status");
        builder.addSection("Availability", "99.95%");
        return builder.build();
    }

    public static void main(String[] args) {
        System.out.println(buildAvailabilityReport(new TextReportBuilder()));
        System.out.println("---");
        System.out.println(buildAvailabilityReport(new HtmlReportBuilder()));
    }
}

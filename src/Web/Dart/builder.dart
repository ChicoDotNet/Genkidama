abstract interface class ReportBuilder {
  void reset();
  void addTitle(String title);
  void addSection(String heading, String body);
  String build();
}

final class TextReportBuilder implements ReportBuilder {
  final List<String> _parts = [];

  @override
  void reset() => _parts.clear();

  @override
  void addTitle(String title) => _parts.add('# $title');

  @override
  void addSection(String heading, String body) =>
      _parts.addAll(['## $heading', body]);

  @override
  String build() => _parts.join('\n');
}

final class HtmlReportBuilder implements ReportBuilder {
  final List<String> _parts = [];

  @override
  void reset() => _parts.clear();

  @override
  void addTitle(String title) => _parts.add('<h1>$title</h1>');

  @override
  void addSection(String heading, String body) =>
      _parts.addAll(['<h2>$heading</h2>', '<p>$body</p>']);

  @override
  String build() => _parts.join();
}

String buildAvailabilityReport(ReportBuilder builder) {
  builder.reset();
  builder.addTitle('Service status');
  builder.addSection('Availability', '99.95%');
  return builder.build();
}

void main() {
  print(buildAvailabilityReport(TextReportBuilder()));
  print('---');
  print(buildAvailabilityReport(HtmlReportBuilder()));
}

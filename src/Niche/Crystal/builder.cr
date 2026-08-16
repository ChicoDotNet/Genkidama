abstract class ReportBuilder
  abstract def reset
  abstract def add_title(title : String)
  abstract def add_section(heading : String, body : String)
  abstract def build : String
end

class TextReportBuilder < ReportBuilder
  @parts = [] of String
  def reset; @parts.clear; end
  def add_title(title : String); @parts << "# #{title}"; end
  def add_section(heading : String, body : String); @parts << "## #{heading}" << body; end
  def build : String; @parts.join("\n"); end
end

class HtmlReportBuilder < ReportBuilder
  @parts = [] of String
  def reset; @parts.clear; end
  def add_title(title : String); @parts << "<h1>#{title}</h1>"; end
  def add_section(heading : String, body : String); @parts << "<h2>#{heading}</h2>" << "<p>#{body}</p>"; end
  def build : String; @parts.join; end
end

def build_availability_report(builder : ReportBuilder) : String
  builder.reset
  builder.add_title("Service status")
  builder.add_section("Availability", "99.95%")
  builder.build
end

puts build_availability_report(TextReportBuilder.new)
puts "---"
puts build_availability_report(HtmlReportBuilder.new)

class TextReportBuilder
  def initialize
    @parts = []
  end

  def reset
    @parts.clear
  end

  def add_title(title)
    @parts << "# #{title}"
  end

  def add_section(heading, body)
    @parts << "## #{heading}"
    @parts << body
  end

  def build
    @parts.join("\n")
  end
end

class HtmlReportBuilder
  def initialize
    @parts = []
  end

  def reset
    @parts.clear
  end

  def add_title(title)
    @parts << "<h1>#{title}</h1>"
  end

  def add_section(heading, body)
    @parts << "<h2>#{heading}</h2>"
    @parts << "<p>#{body}</p>"
  end

  def build
    @parts.join
  end
end

def build_availability_report(builder)
  builder.reset
  builder.add_title('Service status')
  builder.add_section('Availability', '99.95%')
  builder.build
end

puts build_availability_report(TextReportBuilder.new)
puts '---'
puts build_availability_report(HtmlReportBuilder.new)

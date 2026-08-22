class ReportBuilder:
    def __init__(self, format_name):
        self.format_name = format_name
        self.parts = []

    def reset(self):
        self.parts = []

    def add_title(self, title):
        if self.format_name == "text":
            self.parts.append("# " + title)
        else:
            self.parts.append("<h1>" + title + "</h1>")

    def add_section(self, heading, body):
        if self.format_name == "text":
            self.parts.append("## " + heading)
            self.parts.append(body)
        else:
            self.parts.append("<h2>" + heading + "</h2><p>" + body + "</p>")

    def build(self):
        return "\n".join(self.parts)


def build_availability_report(builder):
    builder.reset()
    builder.add_title("Service status")
    builder.add_section("Availability", "99.95%")
    return builder.build()


print(build_availability_report(ReportBuilder("text")))
print("---")
print(build_availability_report(ReportBuilder("html")))

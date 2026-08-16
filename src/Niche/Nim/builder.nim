type
  ReportFormat = enum text, html
  ReportBuilder = object
    format: ReportFormat
    parts: seq[string]

proc reset(builder: var ReportBuilder) = builder.parts.setLen(0)

proc addTitle(builder: var ReportBuilder, title: string) =
  if builder.format == text: builder.parts.add("# " & title)
  else: builder.parts.add("<h1>" & title & "</h1>")

proc addSection(builder: var ReportBuilder, heading, body: string) =
  if builder.format == text:
    builder.parts.add("## " & heading)
    builder.parts.add(body)
  else:
    builder.parts.add("<h2>" & heading & "</h2>")
    builder.parts.add("<p>" & body & "</p>")

proc build(builder: ReportBuilder): string =
  if builder.format == text: builder.parts.join("\n")
  else: builder.parts.join("")

proc buildAvailabilityReport(builder: var ReportBuilder): string =
  builder.reset()
  builder.addTitle("Service status")
  builder.addSection("Availability", "99.95%")
  builder.build()

import std/strutils
var textBuilder = ReportBuilder(format: text)
var htmlBuilder = ReportBuilder(format: html)
echo buildAvailabilityReport(textBuilder)
echo "---"
echo buildAvailabilityReport(htmlBuilder)

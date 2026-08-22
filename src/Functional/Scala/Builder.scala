trait ReportBuilder {
  def reset(): Unit
  def addTitle(title: String): Unit
  def addSection(heading: String, body: String): Unit
  def build(): String
}

final class TextReportBuilder extends ReportBuilder {
  private val parts = scala.collection.mutable.ArrayBuffer.empty[String]
  def reset(): Unit = parts.clear()
  def addTitle(title: String): Unit = parts += s"# $title"
  def addSection(heading: String, body: String): Unit = {
    parts += s"## $heading"
    parts += body
  }
  def build(): String = parts.mkString("\n")
}

final class HtmlReportBuilder extends ReportBuilder {
  private val parts = scala.collection.mutable.ArrayBuffer.empty[String]
  def reset(): Unit = parts.clear()
  def addTitle(title: String): Unit = parts += s"<h1>$title</h1>"
  def addSection(heading: String, body: String): Unit = {
    parts += s"<h2>$heading</h2>"
    parts += s"<p>$body</p>"
  }
  def build(): String = parts.mkString
}

object Builder {
  def buildAvailabilityReport(builder: ReportBuilder): String = {
    builder.reset()
    builder.addTitle("Service status")
    builder.addSection("Availability", "99.95%")
    builder.build()
  }

  def main(args: Array[String]): Unit = {
    println(buildAvailabilityReport(new TextReportBuilder))
    println("---")
    println(buildAvailabilityReport(new HtmlReportBuilder))
  }
}

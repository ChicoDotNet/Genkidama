import scala.collection.mutable

final case class TextStyle(font: String, size: Int, color: String)

final class StyleFactory:
  private val pool = mutable.Map.empty[(String, Int, String), TextStyle]

  def get(font: String, size: Int, color: String): TextStyle =
    pool.getOrElseUpdate((font, size, color), TextStyle(font, size, color))

  def count: Int = pool.size

@main def flyweightExample(): Unit =
  val styles = StyleFactory()
  val red1 = styles.get("Inter", 12, "red")
  val red2 = styles.get("Inter", 12, "red")
  val blue = styles.get("Inter", 12, "blue")
  require(blue.color == "blue")
  val shared = red1 eq red2
  println(s"styles=${styles.count};shared=$shared;text=ABC")

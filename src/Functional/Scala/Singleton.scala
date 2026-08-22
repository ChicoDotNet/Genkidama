object Registry {
  private var countValue = 0

  def increment(): Unit = countValue += 1
  def count: Int = countValue
}

object Singleton extends App {
  val first = Registry
  val second = Registry
  first.increment()

  println(s"same=${first eq second}")
  println(s"count=${second.count}")
}

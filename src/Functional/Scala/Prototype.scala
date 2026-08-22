final case class ServiceProfile(name: String, features: Vector[String]) {
  def cloneProfile: ServiceProfile = copy(features = features.map(identity))
  def describe: String = s"$name: ${features.mkString(",")}"
}

object Prototype extends App {
  val original = ServiceProfile("orders", Vector("metrics"))
  val baseClone = original.cloneProfile
  val canary = baseClone.copy(name = "orders-canary", features = baseClone.features :+ "tracing")

  println(s"original=${original.describe}")
  println(s"clone=${canary.describe}")
}

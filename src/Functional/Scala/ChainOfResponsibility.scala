final case class Handler(
    name: String,
    accepts: Int => Boolean,
    next: Option[Handler]
) {
  def handle(amount: Int, visited: List[String] = Nil): (List[String], String) = {
    val visitedNow = visited :+ name
    if (accepts(amount)) {
      (visitedNow, name)
    } else {
      next match {
        case Some(handler) => handler.handle(amount, visitedNow)
        case None => throw new IllegalStateException("No handler accepted the request")
      }
    }
  }
}

object ChainOfResponsibilityExample extends App {
  val escalation = Handler("escalation", _ => true, None)
  val billing = Handler("billing", _ <= 500, Some(escalation))
  val faq = Handler("faq", _ <= 50, Some(billing))

  val amount = 250
  val (visited, handled) = faq.handle(amount)
  println(
    "visited=" + visited.mkString(">") +
      ";handled=" + handled +
      ";result=refund(" + amount + ")"
  )
}

import scala.collection.mutable

object MediatorExample {
  final class CheckoutMediator {
    private val colleagues = mutable.Map.empty[String, (String, String) => Unit]

    def register(name: String)(receive: (String, String) => Unit): Unit =
      colleagues.update(name, receive)

    def send(sender: String, recipient: String, message: String): Unit =
      colleagues.getOrElse(recipient, throw new IllegalArgumentException(s"unknown colleague: $recipient"))(
        sender,
        message
      )
  }

  def run: Boolean = {
    val events = mutable.ArrayBuffer.empty[String]
    val mediator = new CheckoutMediator

    mediator.register("inventory") { (sender, message) =>
      events += s"inventory<-$sender:$message"
    }
    mediator.register("payment") { (sender, message) =>
      events += s"payment<-$sender:$message"
    }

    mediator.send("payment", "inventory", "paid")
    mediator.send("inventory", "payment", "reserved")

    events.toSeq == Seq("inventory<-payment:paid", "payment<-inventory:reserved")
  }
}

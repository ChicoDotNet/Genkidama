trait Component {
  def render: String
}

final class PlainMessage extends Component {
  override def render: String = "alert"
}

abstract class ComponentDecorator(inner: Component) extends Component {
  protected val wrapped: Component = inner
}

final class AuditDecorator(inner: Component) extends ComponentDecorator(inner) {
  override def render: String = s"audit(${wrapped.render})"
}

final class EncryptDecorator(inner: Component) extends ComponentDecorator(inner) {
  override def render: String = s"enc(${wrapped.render})"
}

object Decorator {
  def main(args: Array[String]): Unit = {
    val component: Component = new PlainMessage
    println(s"base=${component.render}")
    println(s"audit=${new AuditDecorator(component).render}")
    println(s"encrypted=${new EncryptDecorator(component).render}")
    println(s"stacked=${new AuditDecorator(new EncryptDecorator(component)).render}")
  }
}

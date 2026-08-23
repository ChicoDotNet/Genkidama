interface Component { fun render(): String }

class PlainMessage : Component {
    override fun render() = "alert"
}

abstract class ComponentDecorator(protected val inner: Component) : Component

class AuditDecorator(inner: Component) : ComponentDecorator(inner) {
    override fun render() = "audit(${inner.render()})"
}

class EncryptDecorator(inner: Component) : ComponentDecorator(inner) {
    override fun render() = "enc(${inner.render()})"
}

fun main() {
    val base: Component = PlainMessage()
    println("base=${base.render()}")
    println("audit=${AuditDecorator(base).render()}")
    println("encrypted=${EncryptDecorator(base).render()}")
    println("stacked=${AuditDecorator(EncryptDecorator(base)).render()}")
}

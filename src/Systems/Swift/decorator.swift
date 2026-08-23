protocol Component {
    func render() -> String
}

struct PlainMessage: Component {
    func render() -> String { "alert" }
}

class ComponentDecorator: Component {
    let inner: Component

    init(_ inner: Component) {
        self.inner = inner
    }

    func render() -> String {
        inner.render()
    }
}

final class AuditDecorator: ComponentDecorator {
    override func render() -> String { "audit(\(inner.render()))" }
}

final class EncryptDecorator: ComponentDecorator {
    override func render() -> String { "enc(\(inner.render()))" }
}

let base: Component = PlainMessage()
print("base=\(base.render())")
print("audit=\(AuditDecorator(base).render())")
print("encrypted=\(EncryptDecorator(base).render())")
print("stacked=\(AuditDecorator(EncryptDecorator(base)).render())")

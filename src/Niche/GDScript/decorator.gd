extends SceneTree

class Component:
    func render() -> String:
        return ""

class PlainMessage extends Component:
    func render() -> String:
        return "alert"

class ComponentDecorator extends Component:
    var inner: Component
    func _init(component: Component) -> void:
        inner = component

class AuditDecorator extends ComponentDecorator:
    func render() -> String:
        return "audit(%s)" % inner.render()

class EncryptDecorator extends ComponentDecorator:
    func render() -> String:
        return "enc(%s)" % inner.render()

func _init() -> void:
    var base := PlainMessage.new()
    var audited := AuditDecorator.new(base)
    var encrypted := EncryptDecorator.new(base)
    var stacked := AuditDecorator.new(EncryptDecorator.new(base))

    print("base=%s" % base.render())
    print("audit=%s" % audited.render())
    print("encrypted=%s" % encrypted.render())
    print("stacked=%s" % stacked.render())
    quit()

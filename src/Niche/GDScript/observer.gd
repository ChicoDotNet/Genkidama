extends SceneTree

class Publisher:
    extends RefCounted
    signal changed(value: String)

    func subscribe(observer: Callable) -> bool:
        if changed.is_connected(observer):
            return false
        changed.connect(observer)
        return true

    func unsubscribe(observer: Callable) -> bool:
        if not changed.is_connected(observer):
            return false
        changed.disconnect(observer)
        return true

    func publish(value: String) -> void:
        changed.emit(value)

class Observer:
    extends RefCounted
    var messages: Array[String] = []

    func update(value: String) -> void:
        messages.append(value)

func _init() -> void:
    var publisher := Publisher.new()
    var audit := Observer.new()
    var dashboard := Observer.new()
    var audit_callback := Callable(audit, "update")
    var dashboard_callback := Callable(dashboard, "update")

    assert(publisher.subscribe(audit_callback))
    assert(publisher.subscribe(dashboard_callback))
    assert(not publisher.subscribe(audit_callback))

    publisher.publish("draft")
    assert(audit.messages == ["draft"])
    assert(dashboard.messages == ["draft"])

    assert(publisher.unsubscribe(dashboard_callback))
    assert(not publisher.unsubscribe(dashboard_callback))

    publisher.publish("published")
    assert(audit.messages == ["draft", "published"])
    assert(dashboard.messages == ["draft"])

    print("observer=audit:draft,published;dashboard:draft;duplicate=rejected;second-unsubscribe=rejected")
    quit()

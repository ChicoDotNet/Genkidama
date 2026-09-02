extends SceneTree

class Document:
    var state := "draft"
    var tags := ["baseline"]

    func save() -> Dictionary:
        return {
            "state": state,
            "tags": tags.duplicate(true),
        }

    func publish() -> void:
        state = "published"
        tags.append("published")

    func restore(snapshot: Dictionary) -> void:
        state = snapshot.state
        tags = snapshot.tags.duplicate(true)

func _init() -> void:
    var document := Document.new()
    var snapshot := document.save()

    document.publish()
    assert(document.state == "published")
    assert(document.tags == ["baseline", "published"])

    document.restore(snapshot)
    assert(document.state == "draft")
    assert(document.tags == ["baseline"])

    document.tags.append("local-edit")
    assert(snapshot.state == "draft")
    assert(snapshot.tags == ["baseline"])

    print("GDScript Memento: passed")
    quit()

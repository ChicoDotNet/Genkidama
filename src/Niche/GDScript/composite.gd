extends SceneTree

class FileLeaf:
    extends RefCounted
    var bytes: int

    func _init(value: int) -> void:
        bytes = value

    func size() -> int:
        return bytes

class FolderComposite:
    extends RefCounted
    var children: Array

    func _init(items: Array) -> void:
        children = items

    func size() -> int:
        var total := 0
        for child in children:
            total += child.size()
        return total

func _initialize() -> void:
    var readme = FileLeaf.new(2)
    var docs = FolderComposite.new([FileLeaf.new(3), FileLeaf.new(5)])
    var root = FolderComposite.new([readme, docs])

    print("leaf=%d" % readme.size())
    print("docs=%d" % docs.size())
    print("root=%d" % root.size())
    quit()

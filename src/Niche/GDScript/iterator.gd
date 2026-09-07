extends SceneTree

class NumberIterator extends RefCounted:
    var values: Array[int]
    var index := 0

    func _init(source: Array[int]) -> void:
        values = source

    func has_next() -> bool:
        return index < values.size()

    func next_value() -> int:
        assert(has_next(), "Iterator exhausted")
        var current := values[index]
        index += 1
        return current

func _init() -> void:
    var iterator := NumberIterator.new([10, 20, 30])
    var visited: Array[int] = []
    while iterator.has_next():
        visited.append(iterator.next_value())
    if visited != [10, 20, 30] or iterator.has_next():
        push_error("Iterator contract failed")
        quit(1)
        return
    print("iterator=10,20,30")
    quit()

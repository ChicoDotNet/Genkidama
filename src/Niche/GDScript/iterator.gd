extends SceneTree

func make_iterator(values: Array) -> Callable:
    var index := 0
    return func():
        if index >= values.size():
            return null
        var current = values[index]
        index += 1
        return current

func _init() -> void:
    var next := make_iterator([10, 20, 30])
    var visited: Array[int] = []
    while true:
        var current = next.call()
        if current == null:
            break
        visited.append(current)
    if visited != [10, 20, 30] or next.call() != null:
        push_error("Iterator contract failed")
        quit(1)
        return
    print("iterator=10,20,30")
    quit()

extends SceneTree

class Registry:
    var count := 0

var shared_registry := Registry.new()

func instance() -> Registry:
    return shared_registry

func _initialize() -> void:
    var first := instance()
    var second := instance()
    first.count += 1
    print("same=%s" % ("true" if first == second else "false"))
    print("count=%d" % second.count)
    quit()

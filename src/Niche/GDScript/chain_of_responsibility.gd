extends SceneTree

func _init() -> void:
    var handlers := [
        {"name": "faq", "limit": 50},
        {"name": "billing", "limit": 500},
        {"name": "escalation", "limit": 2147483647}
    ]
    var visited: Array[String] = []
    var handled := "none"
    var amount := 250

    for handler in handlers:
        visited.append(handler.name)
        if amount <= handler.limit:
            handled = handler.name
            break

    print("visited=%s;handled=%s;result=refund(%d)" % [">".join(visited), handled, amount])
    quit()

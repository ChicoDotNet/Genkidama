extends SceneTree

func clone_profile(profile: Dictionary) -> Dictionary:
    return profile.duplicate(true)

func describe(profile: Dictionary) -> String:
    return "%s: %s" % [profile["name"], ",".join(profile["features"])]

func _init() -> void:
    var original := {
        "name": "orders",
        "features": ["metrics"],
    }
    var canary := clone_profile(original)
    canary["name"] = "orders-canary"
    canary["features"].append("tracing")

    print("original=" + describe(original))
    print("clone=" + describe(canary))
    quit()

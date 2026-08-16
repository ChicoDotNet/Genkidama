extends SceneTree

# Abstract Factory represented as a family of related constructors.
func create_factory(theme: String) -> Dictionary:
	match theme:
		"dark":
			return {
				"create_button": func() -> void: print("Dark Button"),
				"create_checkbox": func() -> void: print("Dark Checkbox"),
			}
		"light":
			return {
				"create_button": func() -> void: print("Light Button"),
				"create_checkbox": func() -> void: print("Light Checkbox"),
			}
		_:
			push_error("Unknown UI family: %s" % theme)
			return {}


func create_ui_components(factory: Dictionary) -> void:
	var create_button: Callable = factory["create_button"]
	var create_checkbox: Callable = factory["create_checkbox"]
	create_button.call()
	create_checkbox.call()


func _init() -> void:
	create_ui_components(create_factory("dark"))
	create_ui_components(create_factory("light"))
	quit()

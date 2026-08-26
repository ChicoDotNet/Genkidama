extends SceneTree

class TextStyle:
	extends RefCounted
	var font: String
	var size: int
	var color: String

	func _init(p_font: String, p_size: int, p_color: String) -> void:
		font = p_font
		size = p_size
		color = p_color

class StyleFactory:
	var pool: Dictionary = {}

	func get_style(font: String, size: int, color: String) -> TextStyle:
		var key := "%s|%d|%s" % [font, size, color]
		if not pool.has(key):
			pool[key] = TextStyle.new(font, size, color)
		return pool[key]

func _init() -> void:
	var factory := StyleFactory.new()
	var red1 := factory.get_style("Inter", 12, "red")
	var red2 := factory.get_style("Inter", 12, "red")
	var blue := factory.get_style("Inter", 12, "blue")
	assert(blue.color == "blue")
	var shared := "true" if red1 == red2 else "false"
	print("styles=%d;shared=%s;text=ABC" % [factory.pool.size(), shared])
	quit()

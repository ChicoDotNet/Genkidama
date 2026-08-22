extends SceneTree

func create_builder(format: String) -> Dictionary:
	return {"format": format, "parts": []}

func reset(builder: Dictionary) -> void:
	builder.parts.clear()

func add_title(builder: Dictionary, title: String) -> void:
	if builder.format == "text":
		builder.parts.append("# %s" % title)
	else:
		builder.parts.append("<h1>%s</h1>" % title)

func add_section(builder: Dictionary, heading: String, body: String) -> void:
	if builder.format == "text":
		builder.parts.append("## %s" % heading)
		builder.parts.append(body)
	else:
		builder.parts.append("<h2>%s</h2><p>%s</p>" % [heading, body])

func build(builder: Dictionary) -> String:
	return "\n".join(builder.parts)

func build_availability_report(builder: Dictionary) -> String:
	reset(builder)
	add_title(builder, "Service status")
	add_section(builder, "Availability", "99.95%")
	return build(builder)

func _init() -> void:
	print(build_availability_report(create_builder("text")))
	print("---")
	print(build_availability_report(create_builder("html")))
	quit()

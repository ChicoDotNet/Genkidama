extends SceneTree

func create_postgres() -> Dictionary:
	return {
		"connect": func() -> void: print("PostgreSQL connect"),
		"query": func() -> void: print("PostgreSQL query"),
	}

func create_mysql() -> Dictionary:
	return {
		"connect": func() -> void: print("MySQL connect"),
		"query": func() -> void: print("MySQL query"),
	}

func use_database(create_database: Callable) -> void:
	var database: Dictionary = create_database.call()
	database.connect.call()
	database.query.call()

func _init() -> void:
	use_database(create_postgres)
	use_database(create_mysql)
	quit()

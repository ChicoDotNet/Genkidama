extends SceneTree

func authenticate(user: String) -> String:
	return "auth(%s)" % user

func reserve_inventory(sku: String) -> String:
	return "reserve(%s)" % sku

func charge(cents: int) -> String:
	return "charge(%d)" % cents

func checkout(user: String, sku: String, cents: int) -> String:
	return "%s>%s>%s" % [authenticate(user), reserve_inventory(sku), charge(cents)]

func _init() -> void:
	print("checkout=" + checkout("alice", "SKU-42", 499))
	quit()

extends SceneTree

class DocumentStore:
	extends RefCounted
	func get_document(_id: int) -> String:
		return ""

class RemoteDocumentStore:
	extends DocumentStore
	var fetches := 0

	func get_document(id: int) -> String:
		fetches += 1
		return "doc(%d)" % id

class DocumentStoreProxy:
	extends DocumentStore
	var backend_creations := 0
	var backend: RemoteDocumentStore
	var cache: Dictionary = {}

	func get_document(id: int) -> String:
		if cache.has(id):
			return cache[id]
		if backend == null:
			backend = RemoteDocumentStore.new()
			backend_creations += 1
		var value := backend.get_document(id)
		cache[id] = value
		return value

func _init() -> void:
	var store := DocumentStoreProxy.new()
	var first := store.get_document(42)
	var second := store.get_document(42)
	assert(store.backend != null)
	print("backend=%d;fetches=%d;first=%s;second=%s" % [store.backend_creations, store.backend.fetches, first, second])
	quit()

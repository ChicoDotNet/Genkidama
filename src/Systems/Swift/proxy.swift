protocol DocumentStore {
    func get(_ id: Int) -> String
}

final class RemoteDocumentStore: DocumentStore {
    private(set) var fetchCount = 0

    func get(_ id: Int) -> String {
        fetchCount += 1
        return "doc(\(id))"
    }
}

final class DocumentStoreProxy: DocumentStore {
    private var backend: RemoteDocumentStore?
    private var cache: [Int: String] = [:]

    var backendCount: Int { backend == nil ? 0 : 1 }
    var fetchCount: Int { backend?.fetchCount ?? 0 }

    func get(_ id: Int) -> String {
        if let cached = cache[id] {
            return cached
        }
        if backend == nil {
            backend = RemoteDocumentStore()
        }
        let value = backend!.get(id)
        cache[id] = value
        return value
    }
}

let store = DocumentStoreProxy()
let first = store.get(42)
let second = store.get(42)
print("backend=\(store.backendCount);fetches=\(store.fetchCount);first=\(first);second=\(second)")

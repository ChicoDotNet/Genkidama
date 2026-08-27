interface DocumentStore {
    fun get(id: Int): String
}

class RemoteDocumentStore : DocumentStore {
    var fetchCount: Int = 0
        private set

    override fun get(id: Int): String {
        fetchCount += 1
        return "doc($id)"
    }
}

class DocumentStoreProxy : DocumentStore {
    private var backend: RemoteDocumentStore? = null
    private val cache = mutableMapOf<Int, String>()

    val backendCount: Int
        get() = if (backend == null) 0 else 1

    val fetchCount: Int
        get() = backend?.fetchCount ?: 0

    override fun get(id: Int): String {
        cache[id]?.let { return it }
        val real = backend ?: RemoteDocumentStore().also { backend = it }
        return real.get(id).also { cache[id] = it }
    }
}

fun main() {
    val store = DocumentStoreProxy()
    val first = store.get(42)
    val second = store.get(42)
    println("backend=${store.backendCount};fetches=${store.fetchCount};first=$first;second=$second")
}

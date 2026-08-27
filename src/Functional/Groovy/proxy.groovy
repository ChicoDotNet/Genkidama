interface DocumentStore {
    String get(int id)
}

final class RemoteDocumentStore implements DocumentStore {
    int fetches = 0

    @Override
    String get(int id) {
        fetches++
        "doc(${id})"
    }
}

final class DocumentStoreProxy implements DocumentStore {
    private RemoteDocumentStore subject
    private final Map<Integer, String> cache = [:]

    @Override
    String get(int id) {
        cache.computeIfAbsent(id) {
            if (subject == null) {
                subject = new RemoteDocumentStore()
            }
            subject.get(id)
        }
    }

    int backendCount() { subject == null ? 0 : 1 }
    int fetchCount() { subject == null ? 0 : subject.fetches }
}

def store = new DocumentStoreProxy()
def first = store.get(42)
def second = store.get(42)
println "backend=${store.backendCount()};fetches=${store.fetchCount()};first=${first};second=${second}"

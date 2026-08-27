interface DocumentStore {
    String get(int id);
}

final class RemoteDocumentStore implements DocumentStore {
    private int fetchCount;

    @Override
    public String get(int id) {
        fetchCount++;
        return "doc(" + id + ")";
    }

    int fetchCount() {
        return fetchCount;
    }
}

final class DocumentStoreProxy implements DocumentStore {
    private RemoteDocumentStore backend;
    private final java.util.Map<Integer, String> cache = new java.util.HashMap<>();

    @Override
    public String get(int id) {
        var cached = cache.get(id);
        if (cached != null) {
            return cached;
        }
        if (backend == null) {
            backend = new RemoteDocumentStore();
        }
        var value = backend.get(id);
        cache.put(id, value);
        return value;
    }

    int backendCount() {
        return backend == null ? 0 : 1;
    }

    int fetchCount() {
        return backend == null ? 0 : backend.fetchCount();
    }
}

public class ProxyExample {
    public static void main(String[] args) {
        var store = new DocumentStoreProxy();
        var first = store.get(42);
        var second = store.get(42);
        System.out.printf("backend=%d;fetches=%d;first=%s;second=%s%n",
                store.backendCount(), store.fetchCount(), first, second);
    }
}

class RemoteDocumentStore:
    def __init__(self) -> None:
        self.fetch_count = 0

    def get(self, document_id: int) -> str:
        self.fetch_count += 1
        return f"doc({document_id})"


class DocumentStoreProxy:
    def __init__(self) -> None:
        self._backend: RemoteDocumentStore | None = None
        self._cache: dict[int, str] = {}

    @property
    def backend_count(self) -> int:
        return 0 if self._backend is None else 1

    @property
    def fetch_count(self) -> int:
        return 0 if self._backend is None else self._backend.fetch_count

    def get(self, document_id: int) -> str:
        if document_id in self._cache:
            return self._cache[document_id]
        if self._backend is None:
            self._backend = RemoteDocumentStore()
        value = self._backend.get(document_id)
        self._cache[document_id] = value
        return value


store = DocumentStoreProxy()
first = store.get(42)
second = store.get(42)
print(f"backend={store.backend_count};fetches={store.fetch_count};first={first};second={second}")

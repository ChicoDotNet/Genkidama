class RemoteDocumentStore:
    def __init__(self):
        self.fetches = 0

    def get(self, document_id):
        self.fetches += 1
        return "doc({})".format(document_id)


class DocumentStoreProxy:
    def __init__(self):
        self.backend = None
        self.backend_creations = 0
        self.cache = {}

    def get(self, document_id):
        if document_id in self.cache:
            return self.cache[document_id]
        if self.backend is None:
            self.backend = RemoteDocumentStore()
            self.backend_creations += 1
        value = self.backend.get(document_id)
        self.cache[document_id] = value
        return value


store = DocumentStoreProxy()
first = store.get(42)
second = store.get(42)
assert store.backend is not None
print("backend={};fetches={};first={};second={}".format(
    store.backend_creations, store.backend.fetches, first, second))

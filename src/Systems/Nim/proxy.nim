import tables

type
  DocumentStore = ref object of RootObj

  RemoteDocumentStore = ref object of DocumentStore
    fetches: int

  DocumentStoreProxy = ref object of DocumentStore
    backend: RemoteDocumentStore
    cache: Table[int, string]

method getDocument(store: DocumentStore; id: int): string {.base.} =
  raise newException(CatchableError, "abstract document store")

method getDocument(store: RemoteDocumentStore; id: int): string =
  inc store.fetches
  "doc(" & $id & ")"

method getDocument(store: DocumentStoreProxy; id: int): string =
  if store.cache.hasKey(id):
    return store.cache[id]

  if store.backend.isNil:
    store.backend = RemoteDocumentStore(fetches: 0)

  let value = store.backend.getDocument(id)
  store.cache[id] = value
  value

let store = DocumentStoreProxy(cache: initTable[int, string]())
let firstValue = store.getDocument(42)
let secondValue = store.getDocument(42)
let backendCount = if store.backend.isNil: 0 else: 1
let fetchCount = if store.backend.isNil: 0 else: store.backend.fetches

echo "backend=", backendCount, ";fetches=", fetchCount,
  ";first=", firstValue, ";second=", secondValue

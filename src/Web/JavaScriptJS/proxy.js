class RemoteDocumentStore {
  constructor() {
    this.fetchCount = 0;
  }

  get(id) {
    this.fetchCount += 1;
    return `doc(${id})`;
  }
}

class DocumentStoreProxy {
  constructor() {
    this.backend = null;
    this.cache = new Map();
  }

  get backendCount() {
    return this.backend === null ? 0 : 1;
  }

  get fetchCount() {
    return this.backend === null ? 0 : this.backend.fetchCount;
  }

  get(id) {
    if (this.cache.has(id)) return this.cache.get(id);
    if (this.backend === null) this.backend = new RemoteDocumentStore();
    const value = this.backend.get(id);
    this.cache.set(id, value);
    return value;
  }
}

const store = new DocumentStoreProxy();
const first = store.get(42);
const second = store.get(42);
console.log(`backend=${store.backendCount};fetches=${store.fetchCount};first=${first};second=${second}`);

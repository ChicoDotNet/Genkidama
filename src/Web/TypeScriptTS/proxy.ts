interface DocumentStore {
  get(id: number): string;
}

class RemoteDocumentStore implements DocumentStore {
  fetchCount = 0;

  get(id: number): string {
    this.fetchCount += 1;
    return `doc(${id})`;
  }
}

class DocumentStoreProxy implements DocumentStore {
  private backend: RemoteDocumentStore | undefined;
  private readonly cache = new Map<number, string>();

  get backendCount(): number {
    return this.backend === undefined ? 0 : 1;
  }

  get fetchCount(): number {
    return this.backend === undefined ? 0 : this.backend.fetchCount;
  }

  get(id: number): string {
    const cached = this.cache.get(id);
    if (cached !== undefined) return cached;
    if (this.backend === undefined) this.backend = new RemoteDocumentStore();
    const value = this.backend.get(id);
    this.cache.set(id, value);
    return value;
  }
}

const store = new DocumentStoreProxy();
const first = store.get(42);
const second = store.get(42);
console.log(`backend=${store.backendCount};fetches=${store.fetchCount};first=${first};second=${second}`);

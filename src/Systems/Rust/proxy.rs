use std::collections::HashMap;

trait DocumentStore {
    fn get(&mut self, id: u32) -> String;
}

struct RemoteDocumentStore {
    fetch_count: u32,
}

impl RemoteDocumentStore {
    fn new() -> Self {
        Self { fetch_count: 0 }
    }
}

impl DocumentStore for RemoteDocumentStore {
    fn get(&mut self, id: u32) -> String {
        self.fetch_count += 1;
        format!("doc({id})")
    }
}

struct DocumentStoreProxy {
    backend: Option<RemoteDocumentStore>,
    cache: HashMap<u32, String>,
}

impl DocumentStoreProxy {
    fn new() -> Self {
        Self { backend: None, cache: HashMap::new() }
    }

    fn backend_count(&self) -> u32 {
        u32::from(self.backend.is_some())
    }

    fn fetch_count(&self) -> u32 {
        self.backend.as_ref().map_or(0, |backend| backend.fetch_count)
    }
}

impl DocumentStore for DocumentStoreProxy {
    fn get(&mut self, id: u32) -> String {
        if let Some(value) = self.cache.get(&id) {
            return value.clone();
        }
        if self.backend.is_none() {
            self.backend = Some(RemoteDocumentStore::new());
        }
        let value = self.backend.as_mut().expect("backend created").get(id);
        self.cache.insert(id, value.clone());
        value
    }
}

fn main() {
    let mut store = DocumentStoreProxy::new();
    let first = store.get(42);
    let second = store.get(42);
    println!(
        "backend={};fetches={};first={};second={}",
        store.backend_count(), store.fetch_count(), first, second
    );
}

#include <stdio.h>
#include <string.h>

typedef const char *(*GetDocument)(void *context, int id);

typedef struct {
    void *context;
    GetDocument get;
} DocumentStore;

typedef struct {
    int fetch_count;
    char buffer[32];
} RemoteDocumentStore;

static const char *remote_get(void *context, int id) {
    RemoteDocumentStore *store = context;
    store->fetch_count += 1;
    (void)snprintf(store->buffer, sizeof store->buffer, "doc(%d)", id);
    return store->buffer;
}

typedef struct {
    int backend_created;
    RemoteDocumentStore backend;
    int has_cache;
    int cached_id;
    char cached_value[32];
} DocumentStoreProxy;

static const char *proxy_get(void *context, int id) {
    DocumentStoreProxy *proxy = context;
    if (proxy->has_cache && proxy->cached_id == id) {
        return proxy->cached_value;
    }
    proxy->backend_created = 1;
    DocumentStore real = { &proxy->backend, remote_get };
    const char *value = real.get(real.context, id);
    proxy->cached_id = id;
    proxy->has_cache = 1;
    (void)snprintf(proxy->cached_value, sizeof proxy->cached_value, "%s", value);
    return proxy->cached_value;
}

int main(void) {
    DocumentStoreProxy proxy = {0};
    DocumentStore store = { &proxy, proxy_get };
    const char *first = store.get(store.context, 42);
    char first_copy[32];
    (void)snprintf(first_copy, sizeof first_copy, "%s", first);
    const char *second = store.get(store.context, 42);
    printf("backend=%d;fetches=%d;first=%s;second=%s\n",
           proxy.backend_created, proxy.backend.fetch_count, first_copy, second);
    return 0;
}

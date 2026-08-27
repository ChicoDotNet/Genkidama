local RemoteDocumentStore = {}
RemoteDocumentStore.__index = RemoteDocumentStore

function RemoteDocumentStore.new()
  return setmetatable({ fetch_count = 0 }, RemoteDocumentStore)
end

function RemoteDocumentStore:get(id)
  self.fetch_count = self.fetch_count + 1
  return string.format("doc(%d)", id)
end

local DocumentStoreProxy = {}
DocumentStoreProxy.__index = DocumentStoreProxy

function DocumentStoreProxy.new()
  return setmetatable({ backend = nil, cache = {} }, DocumentStoreProxy)
end

function DocumentStoreProxy:get(id)
  local cached = self.cache[id]
  if cached ~= nil then
    return cached
  end

  if self.backend == nil then
    self.backend = RemoteDocumentStore.new()
  end

  local value = self.backend:get(id)
  self.cache[id] = value
  return value
end

function DocumentStoreProxy:backend_count()
  return self.backend == nil and 0 or 1
end

function DocumentStoreProxy:fetch_count()
  return self.backend == nil and 0 or self.backend.fetch_count
end

local store = DocumentStoreProxy.new()
local first = store:get(42)
local second = store:get(42)
print(string.format("backend=%d;fetches=%d;first=%s;second=%s", store:backend_count(), store:fetch_count(), first, second))

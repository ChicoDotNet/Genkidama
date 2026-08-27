class RemoteDocumentStore
  attr_reader :fetch_count

  def initialize
    @fetch_count = 0
  end

  def get(id)
    @fetch_count += 1
    "doc(#{id})"
  end
end

class DocumentStoreProxy
  attr_reader :backend

  def initialize
    @backend = nil
    @cache = {}
  end

  def get(id)
    return @cache[id] if @cache.key?(id)

    @backend ||= RemoteDocumentStore.new
    @cache[id] = @backend.get(id)
  end

  def backend_count
    @backend.nil? ? 0 : 1
  end

  def fetch_count
    @backend&.fetch_count || 0
  end
end

store = DocumentStoreProxy.new
first = store.get(42)
second_value = store.get(42)
puts "backend=#{store.backend_count};fetches=#{store.fetch_count};first=#{first};second=#{second_value}"

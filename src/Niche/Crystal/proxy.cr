abstract class DocumentStore
  abstract def fetch(id : Int32) : String
end

class DocumentBackend < DocumentStore
  getter fetches = 0

  def fetch(id : Int32) : String
    @fetches += 1
    "doc(#{id})"
  end
end

class DocumentProxy < DocumentStore
  getter cache = {} of Int32 => String

  def initialize
    @backend = nil.as(DocumentBackend?)
  end

  def fetch(id : Int32) : String
    @cache[id] ||= begin
      @backend ||= DocumentBackend.new
      @backend.not_nil!.fetch(id)
    end
  end

  def backend_count : Int32
    @backend.nil? ? 0 : 1
  end

  def fetches : Int32
    @backend.try(&.fetches) || 0
  end
end

proxy = DocumentProxy.new
first = proxy.fetch(42)
second = proxy.fetch(42)
puts "backend=#{proxy.backend_count};fetches=#{proxy.fetches};first=#{first};second=#{second}"

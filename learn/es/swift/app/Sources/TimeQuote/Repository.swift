public protocol TimeQuoteRepository {
    mutating func load() throws -> TimeQuoteBook
    mutating func save(_ book: TimeQuoteBook) throws
}

public struct InMemoryTimeQuoteRepository: TimeQuoteRepository {
    private var storedBook: TimeQuoteBook

    public init(initialBook: TimeQuoteBook = TimeQuoteBook()) {
        self.storedBook = initialBook
    }

    public mutating func load() throws -> TimeQuoteBook {
        storedBook
    }

    public mutating func save(_ book: TimeQuoteBook) throws {
        storedBook = book
    }
}

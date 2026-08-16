public struct TimeQuoteService<Repository: TimeQuoteRepository> {
    private var repository: Repository
    private var book: TimeQuoteBook

    public init(repository: Repository) throws {
        var repository = repository
        self.book = try repository.load()
        self.repository = repository
    }

    public mutating func addClient(_ client: Client) throws {
        try book.addClient(client)
        try repository.save(book)
    }

    public mutating func record(_ entry: TimeEntry) throws {
        try book.record(entry)
        try repository.save(book)
    }

    public func summary(for clientID: String) throws -> ClientSummary {
        try book.summary(for: clientID)
    }

    public func allSummaries() -> [ClientSummary] {
        book.allSummaries()
    }
}

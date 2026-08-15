import Foundation

public protocol TimeQuoteRepository {
    mutating func load() throws -> TimeQuoteBook
    mutating func save(_ book: TimeQuoteBook) throws
}

public enum PersistenceError: Error, Equatable, Sendable {
    case readFailed
    case invalidData
    case encodingFailed
    case writeFailed
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

public struct FileTimeQuoteRepository: TimeQuoteRepository {
    private let fileURL: URL
    private let fileManager: FileManager

    public init(fileURL: URL, fileManager: FileManager = .default) {
        self.fileURL = fileURL
        self.fileManager = fileManager
    }

    public mutating func load() throws -> TimeQuoteBook {
        guard fileManager.fileExists(atPath: fileURL.path) else {
            return TimeQuoteBook()
        }

        let data: Data
        do {
            data = try Data(contentsOf: fileURL)
        } catch {
            throw PersistenceError.readFailed
        }

        do {
            return try JSONDecoder().decode(TimeQuoteBook.self, from: data)
        } catch {
            throw PersistenceError.invalidData
        }
    }

    public mutating func save(_ book: TimeQuoteBook) throws {
        let data: Data
        do {
            let encoder = JSONEncoder()
            encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
            data = try encoder.encode(book)
        } catch {
            throw PersistenceError.encodingFailed
        }

        do {
            try fileManager.createDirectory(
                at: fileURL.deletingLastPathComponent(),
                withIntermediateDirectories: true
            )
            try data.write(to: fileURL, options: .atomic)
        } catch {
            throw PersistenceError.writeFailed
        }
    }
}

import Foundation

public struct Client: Equatable, Sendable {
    public let id: String
    public var name: String
    public var hourlyRateCents: Int

    public init(id: String, name: String, hourlyRateCents: Int) throws {
        let cleanID = id.trimmingCharacters(in: .whitespacesAndNewlines)
        let cleanName = name.trimmingCharacters(in: .whitespacesAndNewlines)

        guard !cleanID.isEmpty else { throw TimeQuoteError.invalidClientID }
        guard !cleanName.isEmpty else { throw TimeQuoteError.invalidClientName }
        guard hourlyRateCents > 0 else { throw TimeQuoteError.invalidHourlyRate }

        self.id = cleanID
        self.name = cleanName
        self.hourlyRateCents = hourlyRateCents
    }
}

public struct TimeEntry: Equatable, Sendable {
    public let clientID: String
    public let minutes: Int
    public let note: String?

    public init(clientID: String, minutes: Int, note: String? = nil) throws {
        guard !clientID.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
            throw TimeQuoteError.invalidClientID
        }
        guard minutes > 0 else { throw TimeQuoteError.invalidMinutes }

        self.clientID = clientID
        self.minutes = minutes
        self.note = note?.trimmingCharacters(in: .whitespacesAndNewlines)
    }
}

public enum TimeQuoteError: Error, Equatable, Sendable {
    case invalidClientID
    case invalidClientName
    case invalidHourlyRate
    case invalidMinutes
    case duplicateClient(String)
    case clientNotFound(String)
}

public struct ClientSummary: Equatable, Sendable {
    public let client: Client
    public let minutes: Int
    public let amountCents: Int
}

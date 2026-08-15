public struct TimeQuoteBook: Codable, Sendable {
    private var clients: [String: Client] = [:]
    private var entries: [TimeEntry] = []

    public init() {}

    public mutating func addClient(_ client: Client) throws {
        guard clients[client.id] == nil else {
            throw TimeQuoteError.duplicateClient(client.id)
        }
        clients[client.id] = client
    }

    public mutating func record(_ entry: TimeEntry) throws {
        guard clients[entry.clientID] != nil else {
            throw TimeQuoteError.clientNotFound(entry.clientID)
        }
        entries.append(entry)
    }

    public func summary(for clientID: String) throws -> ClientSummary {
        guard let client = clients[clientID] else {
            throw TimeQuoteError.clientNotFound(clientID)
        }

        let minutes = entries
            .filter { $0.clientID == clientID }
            .reduce(0) { partial, entry in partial + entry.minutes }
        let amountCents = (minutes * client.hourlyRateCents) / 60

        return ClientSummary(client: client, minutes: minutes, amountCents: amountCents)
    }

    public func allSummaries() -> [ClientSummary] {
        clients.values
            .sorted { $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending }
            .map { client in
                let minutes = entries
                    .filter { $0.clientID == client.id }
                    .reduce(0) { partial, entry in partial + entry.minutes }
                return ClientSummary(
                    client: client,
                    minutes: minutes,
                    amountCents: (minutes * client.hourlyRateCents) / 60
                )
            }
    }
}

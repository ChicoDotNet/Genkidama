import Testing
@testable import TimeQuote

@Test func calculatesAmountFromMinutesAndHourlyRate() throws {
    var book = TimeQuoteBook()
    let client = try Client(id: "client-1", name: "Cliente Uno", hourlyRateCents: 60_000)

    try book.addClient(client)
    try book.record(TimeEntry(clientID: client.id, minutes: 30))
    try book.record(TimeEntry(clientID: client.id, minutes: 90))

    let summary = try book.summary(for: client.id)

    #expect(summary.minutes == 120)
    #expect(summary.amountCents == 120_000)
}

@Test func rejectsDuplicateClients() throws {
    var book = TimeQuoteBook()
    let client = try Client(id: "client-1", name: "Cliente Uno", hourlyRateCents: 50_000)
    try book.addClient(client)

    #expect(throws: TimeQuoteError.duplicateClient("client-1")) {
        try book.addClient(client)
    }
}

@Test func rejectsTimeForUnknownClient() throws {
    var book = TimeQuoteBook()
    let entry = try TimeEntry(clientID: "missing", minutes: 30)

    #expect(throws: TimeQuoteError.clientNotFound("missing")) {
        try book.record(entry)
    }
}

@Test func rejectsInvalidDomainValues() {
    #expect(throws: TimeQuoteError.invalidClientName) {
        _ = try Client(id: "client-1", name: "   ", hourlyRateCents: 50_000)
    }
    #expect(throws: TimeQuoteError.invalidMinutes) {
        _ = try TimeEntry(clientID: "client-1", minutes: 0)
    }
}

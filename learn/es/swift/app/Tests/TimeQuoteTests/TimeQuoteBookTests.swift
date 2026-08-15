import Foundation
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

@Test func servicePersistsChangesThroughRepositoryBoundary() throws {
    var service = try TimeQuoteService(repository: InMemoryTimeQuoteRepository())
    let client = try Client(id: "client-2", name: "Cliente Dos", hourlyRateCents: 45_000)

    try service.addClient(client)
    try service.record(TimeEntry(clientID: client.id, minutes: 60, note: "Implementación"))

    let summary = try service.summary(for: client.id)
    #expect(summary.minutes == 60)
    #expect(summary.amountCents == 45_000)
}

@Test func serviceKeepsDomainErrorsExplicit() throws {
    var service = try TimeQuoteService(repository: InMemoryTimeQuoteRepository())
    let entry = try TimeEntry(clientID: "missing", minutes: 15)

    #expect(throws: TimeQuoteError.clientNotFound("missing")) {
        try service.record(entry)
    }
}

@Test func fileRepositorySurvivesRecreation() throws {
    let directory = FileManager.default.temporaryDirectory
        .appendingPathComponent("timequote-\(UUID().uuidString)", isDirectory: true)
    let fileURL = directory.appendingPathComponent("timequote.json")
    defer { try? FileManager.default.removeItem(at: directory) }

    var firstService = try TimeQuoteService(repository: FileTimeQuoteRepository(fileURL: fileURL))
    let client = try Client(id: "persisted", name: "Cliente Persistido", hourlyRateCents: 30_000)
    try firstService.addClient(client)
    try firstService.record(TimeEntry(clientID: client.id, minutes: 90, note: "Persistencia"))

    let secondService = try TimeQuoteService(repository: FileTimeQuoteRepository(fileURL: fileURL))
    let summary = try secondService.summary(for: client.id)

    #expect(summary.minutes == 90)
    #expect(summary.amountCents == 45_000)
}

@Test func fileRepositoryRejectsCorruptDataExplicitly() throws {
    let directory = FileManager.default.temporaryDirectory
        .appendingPathComponent("timequote-corrupt-\(UUID().uuidString)", isDirectory: true)
    let fileURL = directory.appendingPathComponent("timequote.json")
    defer { try? FileManager.default.removeItem(at: directory) }

    try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    try Data("not-json".utf8).write(to: fileURL)
    var repository = FileTimeQuoteRepository(fileURL: fileURL)

    #expect(throws: PersistenceError.invalidData) {
        _ = try repository.load()
    }
}

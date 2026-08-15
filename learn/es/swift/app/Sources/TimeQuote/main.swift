import Foundation

var service: TimeQuoteService<InMemoryTimeQuoteRepository>

do {
    service = try TimeQuoteService(repository: InMemoryTimeQuoteRepository())

    let client = try Client(id: "asbn", name: "ASBN Demo", hourlyRateCents: 50_000)
    try service.addClient(client)
    try service.record(TimeEntry(clientID: client.id, minutes: 90, note: "Arquitectura de solución"))

    let summary = try service.summary(for: client.id)
    let pesos = Decimal(summary.amountCents) / Decimal(100)
    print("TimeQuote")
    print("Cliente: \(summary.client.name)")
    print("Minutos registrados: \(summary.minutes)")
    print("Importe: $\(pesos)")
} catch {
    print("No fue posible calcular el resumen: \(error)")
    exit(EXIT_FAILURE)
}

import Foundation

var book = TimeQuoteBook()

do {
    let client = try Client(id: "asbn", name: "ASBN Demo", hourlyRateCents: 50_000)
    try book.addClient(client)
    try book.record(TimeEntry(clientID: client.id, minutes: 90, note: "Arquitectura de solución"))

    let summary = try book.summary(for: client.id)
    let pesos = Decimal(summary.amountCents) / Decimal(100)
    print("TimeQuote")
    print("Cliente: \(summary.client.name)")
    print("Minutos registrados: \(summary.minutes)")
    print("Importe: $\(pesos)")
} catch {
    print("No fue posible calcular el resumen: \(error)")
    exit(EXIT_FAILURE)
}

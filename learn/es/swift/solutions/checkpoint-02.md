# Solución de referencia — Checkpoint 02

Una solución razonable mantiene el dominio intacto y prueba la colaboración a través del servicio:

```swift
@Test func serviceLoadsExistingRepositoryState() throws {
    var book = TimeQuoteBook()
    let client = try Client(id: "existing", name: "Cliente Existente", hourlyRateCents: 30_000)
    try book.addClient(client)

    var service = try TimeQuoteService(
        repository: InMemoryTimeQuoteRepository(initialBook: book)
    )
    try service.record(TimeEntry(clientID: client.id, minutes: 120))

    let summary = try service.summary(for: client.id)
    #expect(summary.minutes == 120)
    #expect(summary.amountCents == 60_000)
}
```

Lo importante no es copiar el test literalmente. La evidencia clave es que `Client` y `TimeEntry` no conocen el repositorio, `main.swift` consume casos de uso y una futura implementación durable puede reemplazar `InMemoryTimeQuoteRepository` detrás de `TimeQuoteRepository`.

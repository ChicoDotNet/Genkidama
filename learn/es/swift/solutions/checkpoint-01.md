# Solución de referencia — Checkpoint 01

No existe una única respuesta correcta. Una solución razonable conserva el dominio y modifica principalmente `main.swift` y las pruebas.

## Escenario

Puedes registrar tres entradas como 30, 45 y 75 minutos. El total esperado es 150 minutos. A una tarifa de 60,000 centavos por hora, el importe es 150,000 centavos.

La nota de una entrada puede omitirse:

```swift
try book.record(TimeEntry(clientID: client.id, minutes: 45))
```

No uses una cadena vacía sólo para evitar un optional.

## Prueba adicional

Una defensa útil es verificar la tarifa inválida:

```swift
@Test func rejectsZeroHourlyRate() {
    #expect(throws: TimeQuoteError.invalidHourlyRate) {
        _ = try Client(id: "client-1", name: "Cliente", hourlyRateCents: 0)
    }
}
```

## Qué importa de la solución

- El cálculo sigue centralizado en `TimeQuoteBook`.
- Los centavos continúan siendo enteros.
- El caller maneja errores; no los silencia.
- La prueba protege comportamiento observable.

Si tu solución produce la misma evidencia con una estructura igual o más clara, no necesitas copiar esta versión literalmente.

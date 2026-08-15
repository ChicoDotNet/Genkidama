# Lección 15 — Convierte errores en estado visible

## Qué vas a conseguir

Vas a traducir fallos de dominio o persistencia a un estado que una interfaz pueda mostrar sin esconder la causa ni duplicar reglas.

## El problema

Una app profesional no puede limitarse a imprimir un error en consola cuando el usuario intenta registrar tiempo para un cliente inexistente. La capa de presentación necesita recibir un resultado explícito y decidir cómo mostrarlo.

## Concepto

La frontera de aplicación captura errores y cambia `state` a `.failed`:

```swift
public func record(_ entry: TimeEntry) async {
    state = .loading
    await Task.yield()

    do {
        try service.record(entry)
        state = .loaded(service.allSummaries())
    } catch {
        state = .failed(String(describing: error))
    }
}
```

El dominio sigue lanzando `TimeQuoteError`; no conoce alertas, colores ni textos de botones.

## Demostración

La prueba usa una entrada válida sintácticamente pero para un cliente inexistente:

```swift
await application.record(entry)

guard case .failed(let message) = application.state else {
    Issue.record("Expected failed application state")
    return
}
```

[EJECUTAR]

```bash
cd app
swift test
```

## Qué acaba de pasar

El error atraviesa tres capas sin perder semántica:

1. el dominio detecta la violación;
2. el servicio conserva el error;
3. la aplicación lo convierte en estado consumible por UI.

En un producto real, el siguiente paso sería mapear errores técnicos a mensajes localizables. No lo hacemos todavía porque el objetivo de esta lección es separar responsabilidades.

## Errores comunes

- Atrapar todos los errores y devolver éxito vacío.
- Mostrar directamente detalles técnicos sensibles al usuario final.
- Hacer que `Client` o `TimeQuoteBook` construyan mensajes de interfaz.
- Probar únicamente el camino feliz.

## Buenas prácticas

Prueba al menos un failure mode observable. Un sistema que sólo prueba éxito no demuestra cómo se comporta cuando el usuario o la infraestructura fallan.

## Tu turno

Diseña una tabla con tres errores (`duplicateClient`, `clientNotFound`, `invalidData`) y escribe qué debería ver una persona usuaria frente a cada uno. No cambies todavía el dominio.

## Cómo comprobar

Tus mensajes deben ser accionables y no exponer rutas, stack traces ni implementación interna.

## Reto adicional

Propón un enum `PresentationError` separado y explica en qué capa harías el mapeo.

## Resumen

Los errores siguen siendo explícitos, pero ahora la aplicación puede convertirlos en un estado que una UI renderiza de forma determinista.

## Siguiente paso

Continúa con [la lección 16 — Prepara una frontera para SwiftUI](16-frontera-swiftui.md).

## Referencias

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/errorhandling/

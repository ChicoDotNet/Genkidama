# Lección 13 — Haz explícito el trabajo asíncrono

## Qué vas a conseguir

Vas a introducir `async/await` en TimeQuote sin convertir cada función en asíncrona ni contaminar el dominio con detalles de interfaz.

## Antes de empezar

Debes haber completado el bloque 9–12 y poder explicar por qué `TimeQuoteService` no conoce JSON ni rutas de archivos.

## El problema

Una futura interfaz no puede asumir que cargar, guardar o sincronizar siempre será instantáneo. Aunque hoy el repositorio local sea rápido, la frontera de aplicación necesita representar operaciones que pueden suspenderse sin bloquear la interacción.

## Concepto

En Swift, `async` expresa que una operación **puede suspenderse**. No significa automáticamente “crear un hilo”. `await` marca los puntos donde la tarea puede ceder ejecución mientras espera.

La regla de este curso es conservadora: el dominio sigue síncrono porque sus cálculos son inmediatos. La asincronía aparece en la frontera de aplicación, donde sí existe una necesidad de responsividad.

## Demostración

TimeQuote incorpora `TimeQuoteApplication`, aislada al `MainActor`:

```swift
@MainActor
public final class TimeQuoteApplication<Repository: TimeQuoteRepository> {
    public private(set) var state: TimeQuoteViewState = .idle

    public func refresh() async {
        state = .loading
        await Task.yield()
        state = .loaded(service.allSummaries())
    }
}
```

`Task.yield()` no simula una red real; permite observar una frontera asíncrona mínima sin introducir sleeps frágiles ni dependencias externas.

[DEMO]

Ejecuta:

```bash
cd app
swift test
```

La prueba de aplicación debe poder usar `await application.refresh()`.

## Qué acaba de pasar

- el dominio no cambió;
- el repositorio no tuvo que volverse asíncrono por decreto;
- la capa preparada para UI sí expone una operación suspendible;
- el estado puede decir que existe trabajo en curso.

## Errores comunes

- Marcar todo `async` sólo porque Swift ofrece concurrencia.
- Usar `Task.detached` para operaciones que deberían respetar aislamiento.
- Confundir concurrencia con paralelismo.
- Añadir `sleep` para “probar async”.

## Buenas prácticas

Coloca asincronía en las fronteras donde una espera real puede aparecer. Mantén las reglas puras y rápidas tan simples como sea posible.

## Tu turno

Explica qué método de `TimeQuoteBook` **no** convertirías en `async` y por qué.

## Cómo comprobar

Tu respuesta debe distinguir entre cálculo inmediato y operación que puede esperar I/O, red o coordinación externa.

## Reto adicional

Diseña en pseudocódigo un repositorio remoto futuro. Señala qué métodos podrían necesitar `async throws` sin implementarlo todavía.

## Resumen

`async/await` entra porque TimeQuote necesita una frontera responsable ante operaciones potencialmente lentas, no porque sea una característica obligatoria del lenguaje.

## Siguiente paso

Continúa con [la lección 14 — Aísla estado de aplicación con MainActor](14-mainactor-y-estado.md).

## Referencias

- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/

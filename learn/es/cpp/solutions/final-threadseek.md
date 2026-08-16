# Solución de referencia — Evaluación final ThreadSeek

No existe una única solución válida. Esta referencia propone fronteras pequeñas y conserva la arquitectura construida durante el curso.

## Historia 1

Un `DiscoveryFilter` value object puede contener una extensión opcional normalizada. El mismo helper decide si un `FileRecord` candidato debe agregarse; los caminos secuencial y paralelo reutilizan esa decisión.

## Historia 2

El parser debe validar el registro completo, no sólo extraer dos campos y aceptar residuos. Una línea inválida falla con contexto suficiente para diagnosticar el archivo; no se recupera silenciosamente un índice parcial.

## Historia 3

Una frontera `ThreadSeekApplication` puede poseer el `std::stop_source` y crear `DiscoveryOptions` para cada ejecución. El núcleo sólo conoce el `std::stop_token` recibido.

## Historia 4

`DiscoveryProgress` puede crecer con métricas derivadas que se calculen desde contadores atomics. La UI consume snapshots inmutables; no obtiene referencias a buffers locales.

## Historia 5

El experimento debe conservar dataset y contrato, repetir mediciones y reportar mediana o rango. Si 8 workers son peores que 4, ése es un resultado válido, no un fallo que deba esconderse.

## Historia 6

Una futura frontera `ChangeSource` puede emitir cambios normalizados hacia un coordinador incremental. El watcher específico de Windows/Linux/macOS vive detrás de esa frontera; `FileIndex` no necesita conocer APIs de plataforma.

## Lo que no haría todavía

No introduciría un thread pool externo, una base de datos, Rx, Boost.Asio ni watchers nativos sólo para demostrar tecnología. Cada dependencia debe responder a una necesidad observada y conservar una vía de test razonable.

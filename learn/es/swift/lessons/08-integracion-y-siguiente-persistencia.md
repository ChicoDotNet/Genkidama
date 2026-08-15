# Lección 08 — Integra la frontera antes de añadir disco

## Qué vas a conseguir

Verificarás que dominio, servicio y repositorio colaboran sin que la infraestructura se filtre al resto de TimeQuote.

## El problema

Una abstracción sólo aporta valor si el flujo completo sigue funcionando y los errores del dominio continúan siendo visibles.

## Concepto

Las pruebas del servicio comprueban dos contratos: los cambios atraviesan `TimeQuoteRepository` y un cliente inexistente sigue produciendo `TimeQuoteError.clientNotFound`. La frontera no debe tragarse semántica útil.

## Demostración

[EJECUTAR]

```bash
swift test
swift run TimeQuote
```

## Tu turno

Dibuja el flujo `main -> TimeQuoteService -> TimeQuoteBook -> TimeQuoteRepository`. Marca qué componente cambiará cuando agreguemos almacenamiento durable y cuáles deberían permanecer intactos.

## Cómo comprobar

Si para agregar un repositorio de archivo necesitas cambiar `Client`, `TimeEntry` o las firmas públicas del servicio, revisa la frontera antes de seguir.

## Errores comunes

- Declarar una abstracción pero seguir usando la implementación concreta desde todas partes.
- Ocultar errores de negocio dentro del repositorio.
- Introducir SwiftUI sólo para demostrar arquitectura.

## Buenas prácticas

Primero estabiliza el contrato con una implementación sencilla; luego agrega I/O y prueba que la sustitución realmente funciona.

## Resumen

TimeQuote ya está preparado para cambiar almacenamiento sin reescribir el dominio ni la CLI.

## Checkpoint

Realiza [Checkpoint 02](checkpoint-02.md).

## Siguiente paso

El siguiente bloque 9–12 implementará persistencia durable detrás del mismo protocolo y probará que el estado sobrevive entre ejecuciones.

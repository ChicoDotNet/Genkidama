# Curso de Swift desde cero — Construye TimeQuote para horas, clientes y cotizaciones

Swift es un lenguaje moderno, seguro y expresivo creado para construir software desde herramientas de línea de comandos hasta aplicaciones completas para plataformas Apple. Este curso parte desde cero y hace crecer **TimeQuote**, una aplicación para registrar clientes, capturar tiempo de trabajo, calcular importes y preparar cotizaciones.

## Qué vas a construir

TimeQuote comienza como un paquete Swift ejecutable y probado con SwiftPM. Esa base permite aprender tipos, value semantics, colecciones, errores, protocolos y pruebas sin depender de una interfaz gráfica. Cuando el dominio lo justifique, el mismo modelo alimentará una interfaz SwiftUI en macOS/iOS sin duplicar reglas de negocio.

## Estado

**En progreso: 12/17 lecciones.** TimeQuote ya modela clientes y tiempo, calcula importes, protege reglas con pruebas, separa casos de uso de almacenamiento y cuenta con persistencia JSON durable detrás del mismo protocolo.

## Tooling verificado

- Swift 6.3, release estable publicada el 24 de marzo de 2026.
- Swift Package Manager incluido con Swift.
- CI reproducible en Linux mediante la imagen `swift:6.3-noble`.
- Para la etapa SwiftUI se requerirá macOS + Xcode estable; esa excepción de plataforma se introducirá únicamente cuando aporte una capacidad visible.

Swift también dispone de instalación oficial para Windows y Linux, por lo que las primeras competencias del lenguaje no quedan atadas a una Mac.

## Lecciones

1. [Ejecuta TimeQuote y registra tu primer trabajo](lessons/01-primer-registro.md)
2. [Modela clientes y tiempo con tipos de Swift](lessons/02-modela-el-dominio.md)
3. [Colecciones y cálculo de importes](lessons/03-colecciones-y-totales.md)
4. [Errores explícitos y pruebas de comportamiento](lessons/04-errores-y-pruebas.md)
5. [Protocolos como contratos reemplazables](lessons/05-protocolos-y-contratos.md)
6. [Casos de uso sin framework](lessons/06-casos-de-uso.md)
7. [Una implementación en memoria para aprender y probar](lessons/07-repositorio-en-memoria.md)
8. [Integra la frontera antes de añadir disco](lessons/08-integracion-y-siguiente-persistencia.md)
9. [Serializa el estado sin filtrar infraestructura](lessons/09-serializa-el-estado.md)
10. [Repositorio JSON durable](lessons/10-repositorio-json-durable.md)
11. [Fallos de I/O explícitos](lessons/11-fallos-de-io-explicitos.md)
12. [Prueba persistencia entre instancias](lessons/12-prueba-persistencia-entre-instancias.md)

### Checkpoints

- [Checkpoint 01 — Un cliente, varias horas y un total verificable](lessons/checkpoint-01.md)
- [Checkpoint 02 — Sustituye almacenamiento sin tocar el dominio](lessons/checkpoint-02.md)
- [Checkpoint 03 — Haz durable TimeQuote](lessons/checkpoint-03.md)

## Instalar, build, test y run

Desde `app/`:

```bash
swift --version
swift build
swift test
swift run TimeQuote
```

En Windows o Linux instala Swift siguiendo las instrucciones oficiales de Swift.org. En macOS puedes usar el toolchain incluido con Xcode o una instalación oficial de Swift.

## Qué aprenderás en el curso completo

La ruta cubrirá progresivamente sintaxis y tipos, structs/enums, optionals, colecciones, funciones, protocolos, errores, persistencia local, concurrencia con async/await, pruebas, SwiftUI, arquitectura de estado y una evaluación final que extiende la misma aplicación sin receta paso a paso.

## Contexto profesional

Swift se usa especialmente para aplicaciones iOS y macOS, pero el lenguaje y SwiftPM también soportan herramientas, librerías y servicios. El objetivo aquí es que puedas leer, modificar, probar y explicar una base Swift pequeña con hábitos profesionales razonables; no prometer empleo.

## Curso transversal de Git

Para ramas, historial, recuperación y colaboración utiliza el [curso transversal de Git](../git/README.md). Aquí sólo mostraremos comandos de Git cuando sean inevitables para obtener o ejecutar material.

## Referencias oficiales

- https://www.swift.org/getting-started/
- https://www.swift.org/blog/swift-6.3-released/
- https://docs.swift.org/swift-book/documentation/the-swift-programming-language/
- https://developer.apple.com/xcode/system-requirements

## Siguiente paso

Si empiezas desde cero, comienza por [la lección 1](lessons/01-primer-registro.md). Si ya completaste el segundo checkpoint, continúa con [la lección 9](lessons/09-serializa-el-estado.md).

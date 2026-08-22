# Decorator

> **Familia:** Structural  
> **Intención:** añadir responsabilidades a un objeto de forma componible, envolviéndolo con objetos que conservan el mismo contrato observable.  
> **Estado:** `in-progress`  
> **Implementaciones de lenguaje:** `0/48`  
> **Cobertura de pruebas:** N/A — la completitud de lenguajes se valida por comportamiento/toolchain; no existe una métrica homogénea entre 48 ecosistemas standalone.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Decorator permite apilar comportamiento alrededor de un componente sin cambiar al cliente ni crear una subclase por cada combinación de responsabilidades.

## El problema

Un componente cumple un contrato útil, pero algunos clientes necesitan capacidades adicionales —por ejemplo auditoría, compresión, caché, autorización o métricas— en combinaciones que cambian en runtime. Si cada combinación se modela con una subclase (`AuditEncryptedComponent`, `CachedAuthorizedComponent`, etc.), el número de tipos crece combinatoriamente y las responsabilidades quedan acopladas entre sí.

## Fuerzas que compiten

- El cliente debería seguir dependiendo del mismo contrato del componente.
- Las responsabilidades opcionales deben poder combinarse sin una explosión de subclases.
- El orden de los wrappers puede importar y debe ser visible en el diseño.
- Añadir demasiadas capas pequeñas puede dificultar trazabilidad, debugging y configuración.

## La solución

Definir un contrato común `Component`. El componente base implementa el comportamiento esencial. Cada Decorator implementa el mismo contrato, conserva una referencia al componente envuelto y añade responsabilidad antes o después de delegar. Como un Decorator también es un `Component`, varios wrappers pueden apilarse dinámicamente.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Component` | Contrato que el cliente consume tanto para componentes base como decorados. |
| `ConcreteComponent` | Implementa el comportamiento esencial sin responsabilidades opcionales. |
| `Decorator` | Conserva un `Component` envuelto y mantiene el mismo contrato. |
| `ConcreteDecorator` | Añade una responsabilidad concreta y delega al componente envuelto. |
| Cliente | Compone wrappers según las responsabilidades necesarias y usa sólo `Component`. |

## Cómo funciona

1. El cliente crea un componente base.
2. Si necesita una responsabilidad adicional, lo envuelve con un Decorator compatible.
3. Cada Decorator conserva el mismo contrato y delega al objeto envuelto.
4. Se pueden apilar varios Decorators; cada uno añade su responsabilidad en un orden explícito.
5. El cliente invoca la operación común sin conocer la clase concreta de cada capa.

## Diagrama

```mermaid
classDiagram
    class Component {
      <<interface>>
      +render() string
    }
    class PlainMessage {
      +render() string
    }
    class Decorator {
      -Component inner
      +render() string
    }
    class AuditDecorator {
      +render() string
    }
    class EncryptDecorator {
      +render() string
    }

    Component <|.. PlainMessage
    Component <|.. Decorator
    Decorator <|-- AuditDecorator
    Decorator <|-- EncryptDecorator
    Decorator o--> Component : wraps
```

La relación importante es que el wrapper **sigue siendo consumible como `Component`**. Esa sustitución permite apilar responsabilidades sin cambiar al cliente.

## Ejemplo mínimo

```text
base = PlainMessage("alert")
audited = AuditDecorator(base)
encrypted = EncryptDecorator(base)
stacked = AuditDecorator(EncryptDecorator(base))

base.render()      // alert
audited.render()   // audit(alert)
encrypted.render() // enc(alert)
stacked.render()   // audit(enc(alert))
```

El ejemplo canónico de este patrón en Genkidama usa precisamente estas cuatro observaciones para demostrar componente base, dos responsabilidades independientes y composición de wrappers.

## Aplicación real

### Responsabilidades transversales combinables

Un servicio puede necesitar logging, caché, validación o autorización según el contexto. Decorator permite envolver el mismo contrato de servicio con estas responsabilidades sin que el servicio base conozca cada preocupación transversal ni crear una clase por cada combinación.

Si todas las llamadas necesitan siempre exactamente la misma política y nunca varía la combinación, una composición explícita más simple o middleware dedicado puede ser suficiente.

## En Genkidama

La filosofía del repositorio identifica **logging, caching, validation y authorization wrappers** como presión natural para Decorator, pero esta página no declara una implementación productiva deliberada sin una ruta concreta y auditada. El catálogo educativo no modificará arquitectura productiva sólo para exhibir el patrón.

## Cuándo usarlo

- Varias responsabilidades opcionales deben combinarse dinámicamente alrededor del mismo contrato.
- Herencia produciría una explosión de subclases por combinaciones.
- El cliente debe poder tratar componentes decorados y no decorados de forma uniforme.
- Es valioso añadir comportamiento sin modificar el componente base.

## Cuándo no usarlo

- Sólo existe una responsabilidad fija y una composición directa es más clara.
- El wrapper cambia el contrato en vez de preservarlo; considera Adapter.
- El objetivo principal es controlar acceso, ubicación o ciclo de vida de otro objeto; considera Proxy.
- La intención es simplificar un subsistema amplio detrás de una API menor; considera Facade.

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Evita explosión de subclases por combinaciones. | Muchas capas pequeñas pueden complicar debugging. |
| Permite combinar responsabilidades en runtime. | El orden de wrappers puede alterar comportamiento y debe documentarse. |
| Mantiene al cliente sobre un contrato estable. | Configurar stacks complejos puede requerir factories/DI. |
| Favorece responsabilidades pequeñas y aisladas. | La identidad del objeto envuelto puede quedar menos evidente. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [Composite](Composite.md) | often implemented with | Ambos comparten un contrato recursivo/compuesto; Decorator suele envolver un único componente mientras Composite agrupa varios. |
| [Proxy](Proxy.md) | often confused with | Ambos envuelven el mismo contrato, pero Proxy controla acceso/ubicación y Decorator añade responsabilidades. |
| [Adapter](Adapter.md) | often confused with | Adapter cambia el contrato para compatibilidad; Decorator preserva el contrato para añadir comportamiento. |
| [Strategy](Strategy.md) | alternative to | Strategy sustituye una política interna; Decorator apila responsabilidades externas alrededor del componente. |

## Errores comunes y confusiones

### Llamar Decorator a cualquier wrapper

Un wrapper sólo es Decorator cuando conserva el contrato del componente y añade una responsabilidad componible. Si traduce una interfaz, es Adapter; si controla acceso a un sujeto, suele ser Proxy.

### Esconder orden significativo

`Audit(Encrypt(component))` y `Encrypt(Audit(component))` pueden producir efectos diferentes. Si el orden importa, debe tratarse como parte del diseño/configuración y verificarse.

### Convertir cada método en una cadena ceremonial

Si no existe presión real de combinación dinámica, una función o composición directa puede ser más clara que una jerarquía de Decorators.

## Cómo comprobar una implementación

- El componente base y los decorados se consumen mediante el mismo contrato.
- Cada Decorator delega al componente envuelto y añade una responsabilidad observable.
- Dos responsabilidades pueden aplicarse de forma independiente.
- Dos Decorators pueden apilarse y el resultado refleja el orden de composición.
- La prueba observa comportamiento, no nombres como `Decorator` o herencia.

## Matriz de implementaciones

El universo canónico mantiene **51 targets**. Decorator clasifica provisionalmente **48 como Applicable** y **HTML, CSS y SQL declarativo como N/A**. La falta de clases no excluye ningún lenguaje: closures, higher-order functions, records, modules, predicates, tables, callbacks y otros mecanismos son válidos si preservan el contrato y la composición.

| Lenguaje / target | Aplicabilidad | Ejemplo | Estado |
|---|---|---|---|
| C# | Applicable | [`src/Enterprise/C#/DecoratorExample.cs`](../src/Enterprise/C%23/DecoratorExample.cs) | candidato |
| TypeScript | Applicable | [`src/Web/TypeScriptTS/decorator.ts`](../src/Web/TypeScriptTS/decorator.ts) | candidato |
| Python | Applicable | [`src/Scripting/PythonPY/decorator.py`](../src/Scripting/PythonPY/decorator.py) | candidato |
| C++ | Applicable | [`src/Systems/C++/decorator.cpp`](../src/Systems/C%2B%2B/decorator.cpp) | candidato |
| Java | Applicable | [`src/Enterprise/Java/DecoratorExample.java`](../src/Enterprise/Java/DecoratorExample.java) | candidato |
| Rust | Applicable | [`src/Systems/Rust/decorator.rs`](../src/Systems/Rust/decorator.rs) | candidato |
| Go | Applicable | [`src/Systems/Go/decorator.go`](../src/Systems/Go/decorator.go) | candidato |
| PHP | Applicable | [`src/Scripting/PHP/decorator.php`](../src/Scripting/PHP/decorator.php) | candidato |
| Kotlin | Applicable | [`src/Enterprise/Kotlin/DecoratorExample.kt`](../src/Enterprise/Kotlin/DecoratorExample.kt) | candidato |
| Swift | Applicable | [`src/Systems/Swift/decorator.swift`](../src/Systems/Swift/decorator.swift) | candidato |
| F# | Applicable | [`src/Functional/F#/decorator.fsx`](../src/Functional/F%23/decorator.fsx) | candidato |
| JavaScript | Applicable | [`src/Web/JavaScriptJS/decorator.js`](../src/Web/JavaScriptJS/decorator.js) | candidato |
| Visual Basic .NET | Applicable | — | pendiente |
| C | Applicable | — | pendiente |
| Ruby | Applicable | — | pendiente |
| Lua | Applicable | — | pendiente |
| Bash | Applicable | — | pendiente |
| PowerShell | Applicable | — | pendiente |
| Haskell | Applicable | — | pendiente |
| Scala | Applicable | — | pendiente |
| Perl | Applicable | — | pendiente |
| Pascal | Applicable | — | pendiente |
| R | Applicable | — | pendiente |
| GNU Octave | Applicable | — | pendiente |
| Julia | Applicable | — | pendiente |
| OCaml | Applicable | — | pendiente |
| Common Lisp | Applicable | — | pendiente |
| Clojure | Applicable | — | pendiente |
| Elixir | Applicable | — | pendiente |
| Erlang | Applicable | — | pendiente |
| Prolog | Applicable | — | pendiente |
| Groovy | Applicable | — | pendiente |
| Ada | Applicable | — | pendiente |
| Solidity | Applicable | — | pendiente |
| Fortran | Applicable | — | pendiente |
| Objective-C | Applicable | — | pendiente |
| Zig | Applicable | — | pendiente |
| Nim | Applicable | — | pendiente |
| Dart | Applicable | — | pendiente |
| Crystal | Applicable | — | pendiente |
| COBOL | Applicable | — | pendiente |
| VBA | Applicable | — | pendiente |
| GDScript | Applicable | — | pendiente |
| MATLAB | Applicable | — | pendiente |
| Assembly | Applicable | — | pendiente |
| Delphi | Applicable | — | pendiente |
| MicroPython | Applicable | — | pendiente |
| Rockstar | Applicable | — | pendiente |
| HTML | N/A | — | Declarativo: el comportamiento ejecutable pertenece al runtime/script que procesa el markup. |
| CSS | N/A | — | Declarativo: reglas de presentación no proporcionan por sí mismas objetos/runtime wrappers componibles. |
| SQL | N/A | — | SQL declarativo puede transformar datos, pero no expresa por sí mismo el contrato runtime que un Decorator preserva y envuelve. |

Un ejemplo faltante o no verificado mantiene esta página `in-progress`; nunca se sustituye por un enlace inventado.

## Comprueba que lo entendiste

1. ¿Qué propiedad distingue a Decorator de Adapter aunque ambos puedan envolver otro objeto?
2. ¿Por qué `Audit(Encrypt(component))` puede no ser equivalente a `Encrypt(Audit(component))`?
3. ¿En qué situación una composición simple sería preferible a una cadena de Decorators?

## Resumen

- **Presión:** añadir responsabilidades combinables sin multiplicar subclases.
- **Movimiento:** envolver un `Component` con objetos que conservan su contrato y delegan.
- **Trade-off:** flexibilidad composicional a cambio de más capas e importancia del orden.
- **Distinción clave:** Decorator preserva contrato para añadir responsabilidad; Adapter cambia contrato y Proxy controla acceso.
- **Portabilidad:** no requiere OOP; cualquier mecanismo que preserve contrato, delegación y composición puede expresar la intención.

## Referencias

- Gamma, Helm, Johnson y Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software* — Decorator.
- [`001-patterns-as-living-examples.md`](../docs/philosophy/001-patterns-as-living-examples.md).
- [`pattern-authoring-standard.md`](../docs/kb/catalog/pattern-authoring-standard.md) — KB-006.

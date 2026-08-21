# Bridge

> **Familia:** Structural  
> **Intención:** separar una abstracción de su implementación para que ambas dimensiones puedan variar y evolucionar de forma independiente.  
> **Estado:** `in-progress`  
> **Implementaciones de lenguaje:** `0/48`  
> **Cobertura de pruebas:** N/A — los ejemplos standalone se validan con compile/run/análisis proporcional; no existe una métrica homogénea defendible entre ecosistemas.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Bridge evita multiplicar combinaciones de tipos cuando tenemos **dos ejes de variación independientes**: la abstracción que usa el cliente y la implementación que realiza el trabajo.

## El problema

Supongamos controles remotos y dispositivos. Si codificamos cada combinación como una clase distinta (`BasicTvRemote`, `BasicRadioRemote`, `MuteTvRemote`, `MuteRadioRemote`), cada nuevo control o dispositivo multiplica la jerarquía. El problema no es sólo reutilización: son dos dimensiones que deberían poder cambiar sin obligarse mutuamente.

## Fuerzas que compiten

- El cliente necesita una abstracción estable sin conocer detalles concretos del dispositivo.
- Abstracciones e implementaciones deben poder crecer de forma independiente.
- Heredar cada combinación produce explosión combinatoria y acoplamiento estructural.
- Introducir una frontera adicional para una sola combinación fija añade complejidad innecesaria.

## La solución

Mover una dimensión de variación a una composición explícita. La **Abstraction** mantiene una referencia a una **Implementor** y expresa operaciones de alto nivel; implementaciones concretas resuelven la parte dependiente de plataforma/dispositivo. Las abstracciones refinadas pueden añadir comportamiento sin crear una subclase por cada implementador.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Abstraction` | Expone la interfaz que usa el cliente y delega trabajo dependiente a `Implementor`. |
| `RefinedAbstraction` | Añade variantes de comportamiento sobre la misma frontera de implementación. |
| `Implementor` | Define las operaciones primitivas que requiere la abstracción. |
| `ConcreteImplementor` | Implementa esas operaciones para una plataforma/dispositivo concreto. |
| Cliente | Elige independientemente abstracción e implementación y las compone. |

## Cómo funciona

1. El cliente selecciona un implementador concreto, por ejemplo `TvDevice` o `RadioDevice`.
2. Construye una abstracción, por ejemplo `BasicRemote` o `MuteRemote`, pasando el implementador.
3. La abstracción decide el flujo de alto nivel y delega las operaciones dependientes al implementador.
4. Cualquiera de los dos ejes puede incorporar nuevas variantes sin crear el producto cartesiano de clases.

## Diagrama

```mermaid
classDiagram
    class RemoteControl {
      -Device device
      +activate() string
    }
    class BasicRemote
    class MuteRemote
    class Device {
      <<interface>>
      +powerOn() string
      +mute() string
    }
    class TvDevice
    class RadioDevice

    RemoteControl <|-- BasicRemote
    RemoteControl <|-- MuteRemote
    RemoteControl o--> Device
    Device <|.. TvDevice
    Device <|.. RadioDevice
```

El punto importante es la composición `RemoteControl → Device`: las dos jerarquías no se cruzan mediante una clase por combinación.

## Ejemplo mínimo

```text
tv = TvDevice()
radio = RadioDevice()

print(BasicRemote(tv).activate())   // TV:on
print(BasicRemote(radio).activate()) // Radio:on
print(MuteRemote(tv).activate())    // TV:muted
print(MuteRemote(radio).activate()) // Radio:muted
```

## Aplicación real

### APIs de alto nivel sobre proveedores intercambiables

Una abstracción de almacenamiento puede exponer operaciones de negocio estables mientras implementadores separados hablan con Azure Blob, S3 o filesystem local. Si además existen abstracciones refinadas —por ejemplo lectura simple y lectura auditada— Bridge evita combinar cada modo de uso con cada proveedor mediante herencia.

Si sólo necesitamos convertir una API incompatible a otra, Adapter es más apropiado. Si queremos ocultar una API grande tras una entrada simple, Facade describe mejor la presión.

## En Genkidama

La filosofía del repositorio reconoce Bridge como una frontera potencialmente útil cuando dos dimensiones cambian de forma independiente, pero esta auditoría no identifica todavía un uso productivo deliberado que deba presentarse como canónico. No se modificará arquitectura productiva para fabricar uno.

## Cuándo usarlo

- Existen dos ejes de variación independientes y ambos crecerán.
- Una jerarquía empieza a multiplicar clases por combinaciones de abstracción/plataforma.
- El cliente debe depender de una abstracción estable mientras cambia la implementación subyacente.

## Cuándo no usarlo

- Sólo existe una implementación y no hay presión real de una segunda dimensión.
- La necesidad es adaptar una interfaz heredada incompatible: usa Adapter.
- Sólo buscamos esconder complejidad detrás de una API simple: considera Facade.

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Evita explosión combinatoria de subclases. | Añade una indirección y más participantes. |
| Permite evolucionar ambos ejes independientemente. | Exige identificar correctamente cuáles dimensiones son realmente independientes. |
| Facilita sustituir implementadores en runtime o composición. | Puede ser sobreingeniería si la segunda dimensión nunca varía. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [Adapter](Adapter.md) | often confused with | Adapter repara una incompatibilidad existente; Bridge diseña dos dimensiones para variar independientemente. |
| [Abstract Factory](AbstractFactory.md) | collaborates with | Una factory puede seleccionar el implementador concreto que una abstracción Bridge consumirá. |
| [Strategy](Strategy.md) | often confused with | Strategy varía un algoritmo/comportamiento intercambiable; Bridge separa dos jerarquías o dimensiones estructurales. |
| [Facade](Facade.md) | often confused with | Facade simplifica una superficie; Bridge desacopla abstracción e implementación. |

## Errores comunes y confusiones

### Llamar Bridge a cualquier composición

Composición por sí sola no demuestra Bridge. Deben existir dos dimensiones de variación con razones independientes para cambiar.

### Confundirlo con Adapter

Adapter suele aparecer después de que dos contratos incompatibles ya existen. Bridge es una decisión de diseño para evitar acoplar desde el principio una abstracción a una familia concreta de implementaciones.

### Crear la segunda jerarquía sin necesidad

Si sólo existe una implementación estable y no hay evidencia de otra dimensión, una interfaz adicional puede empeorar claridad sin aportar flexibilidad.

## Cómo comprobar una implementación

- La misma abstracción funciona con al menos dos implementadores concretos.
- Al menos dos variantes de abstracción pueden reutilizar los mismos implementadores sin clases por combinación.
- El cliente no necesita `switch`/`if` sobre el tipo concreto dentro de la abstracción estable.
- Añadir una nueva abstracción o implementación no obliga a editar todas las combinaciones existentes.

## Implementaciones por lenguaje

La tabla es autoritativa para la completitud de lenguaje. El universo canónico mantiene **51 targets**: **48 Applicable** y **3 N/A**.

| Lenguaje | Aplicabilidad | Ejemplo verificado | Validación | Nota |
|---|---|---|---|---|
| C# | Applicable | [BridgeExample.cs](../src/Enterprise/C%23/BridgeExample.cs) | candidato — gate inicial | interfaces + composición |
| TypeScript | Applicable | [bridge.ts](../src/Web/TypeScriptTS/bridge.ts) | candidato — gate inicial | structural typing |
| Python | Applicable | [bridge.py](../src/Scripting/PythonPY/bridge.py) | candidato — gate inicial | protocols por duck typing |
| C++ | Applicable | [bridge.cpp](../src/Systems/C++/bridge.cpp) | candidato — gate inicial | interfaces abstractas + referencias |
| Java | Applicable | [BridgeExample.java](../src/Enterprise/Java/BridgeExample.java) | candidato — gate inicial | interfaces + composición |
| Rust | Applicable | [bridge.rs](../src/Systems/Rust/bridge.rs) | candidato — gate inicial | trait + generic bridge |
| Go | Applicable | [bridge.go](../src/Systems/Go/bridge.go) | candidato — gate inicial | interfaces implícitas |
| PHP | Applicable | [bridge.php](../src/Scripting/PHP/bridge.php) | candidato — gate inicial | interfaces + composición |
| Kotlin | Applicable | [BridgeExample.kt](../src/Enterprise/Kotlin/BridgeExample.kt) | candidato — gate inicial | interfaces + composición |
| Swift | Applicable | [bridge.swift](../src/Systems/Swift/bridge.swift) | candidato — gate inicial | protocols + composición |
| F# | Applicable | [bridge.fsx](../src/Functional/F%23/bridge.fsx) | candidato — gate inicial | records de funciones |
| JavaScript | Applicable | [bridge.js](../src/Web/JavaScriptJS/bridge.js) | candidato — gate inicial | objetos por composición |
| Visual Basic .NET | Applicable | — | pendiente de implementación | interfaces/clases |
| C | Applicable | — | pendiente de implementación | structs + function pointers |
| Ruby | Applicable | — | pendiente de implementación | duck typing/composición |
| Lua | Applicable | — | pendiente de implementación | tables + closures |
| Bash | Applicable | — | pendiente de implementación | funciones + dispatch explícito |
| PowerShell | Applicable | — | pendiente de implementación | scriptblocks/objetos |
| Haskell | Applicable | — | pendiente de implementación | records de funciones |
| Scala | Applicable | — | pendiente de implementación | traits + composición |
| Perl | Applicable | — | pendiente de implementación | packages/closures |
| Pascal | Applicable | — | pendiente de implementación | interfaces/records procedurales |
| R | Applicable | — | pendiente de implementación | closures/listas |
| GNU Octave | Applicable | — | pendiente de implementación | function handles/structs |
| Julia | Applicable | — | pendiente de implementación | multiple dispatch/composición |
| OCaml | Applicable | — | pendiente de implementación | modules/records de funciones |
| Common Lisp | Applicable | — | pendiente de implementación | closures/structures |
| Clojure | Applicable | — | pendiente de implementación | maps/protocol-like functions |
| Elixir | Applicable | — | pendiente de implementación | behaviours/modules |
| Erlang | Applicable | — | pendiente de implementación | modules/tuples |
| Prolog | Applicable | — | pendiente de implementación | predicates/terms |
| Groovy | Applicable | — | pendiente de implementación | interfaces/duck typing |
| Ada | Applicable | — | pendiente de implementación | tagged types/access operations |
| Solidity | Applicable | — | pendiente de implementación | interfaces/contracts |
| Fortran | Applicable | — | pendiente de implementación | derived types/procedure pointers |
| Objective-C | Applicable | — | pendiente de implementación | protocols/composition |
| Zig | Applicable | — | pendiente de implementación | structs/function pointers |
| Nim | Applicable | — | pendiente de implementación | objects/procs |
| Dart | Applicable | — | pendiente de implementación | abstract classes/composition |
| Crystal | Applicable | — | pendiente de implementación | abstract classes/composition |
| COBOL | Applicable | — | pendiente de implementación | records + dynamic calls |
| VBA | Applicable | — | pendiente de implementación | class modules/interfaces |
| GDScript | Applicable | — | pendiente de implementación | objects/composition |
| MATLAB | Applicable | — | pendiente de implementación | handle/value objects/function handles |
| Assembly | Applicable | — | pendiente de implementación | tables + function pointers |
| Delphi | Applicable | — | pendiente de implementación | interfaces/classes |
| MicroPython | Applicable | — | pendiente de implementación | objects/composition |
| Rockstar | Applicable | — | pendiente de implementación | keyed arrays + explicit dispatch |
| HTML | N/A | — | — | markup declarativo; cualquier bridge ejecutable pertenece al runtime que lo procesa. |
| CSS | N/A | — | — | reglas declarativas de presentación sin abstracciones/runtime calls que desacoplar. |
| SQL | N/A | — | — | SQL declarativo transforma/consulta datos, pero no expresa por sí solo dos dimensiones runtime de abstracción e implementación. |

## Comprueba que lo entendiste

1. Si cada nuevo dispositivo obliga a crear una subclase por cada tipo de control remoto, ¿qué fuerza indica que Bridge puede ayudar?
2. ¿Por qué un wrapper que convierte Fahrenheit a Celsius es Adapter y no Bridge?
3. ¿Cuándo sería más simple mantener una sola interfaz concreta en vez de introducir Bridge?

## Resumen

- **Presión:** dos dimensiones independientes empiezan a multiplicar combinaciones.
- **Movimiento:** una abstracción compone un implementador en vez de heredar cada combinación.
- **Trade-off:** más indirección a cambio de evolución independiente.
- **Relación clave:** Adapter corrige incompatibilidades; Bridge separa dimensiones desde el diseño.
- **Portabilidad:** clases no son requisito; records de funciones, modules, closures, traits o tablas pueden preservar la intención.

## Referencias

- Gamma, Helm, Johnson y Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software* — Bridge.
- [Patterns as Living Examples](../docs/philosophy/001-patterns-as-living-examples.md).
- [Canonical Design Pattern Authoring Standard](../docs/kb/catalog/pattern-authoring-standard.md).

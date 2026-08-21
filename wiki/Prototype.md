# Prototype

> **Familia:** Creational  
> **Intención:** crear nuevos objetos copiando una instancia prototipo existente cuando reutilizar su estado configurado resulta más claro o económico que reconstruirlo desde cero.  
> **Estado:** `validated`  
> **Implementaciones de lenguaje:** `48/48`  
> **Cobertura de pruebas:** N/A — no existe una métrica homogénea entre los ejemplos standalone; se usará la evidencia más fuerte razonablemente disponible por target.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Prototype parte de un objeto ya configurado y crea una copia independiente que puede variar sin reconstruir toda su configuración ni acoplar al cliente a un constructor concreto.

## El problema

Algunas instancias son costosas o verbosas de configurar: contienen muchas opciones, una estructura interna preparada, defaults derivados o colecciones que deben comenzar con un estado coherente. Si cada cliente reconstruye esa configuración desde cero, duplica conocimiento y aumenta el riesgo de inconsistencias.

También puede ocurrir que el cliente conozca la abstracción que necesita pero no deba conocer la clase concreta ni todos los detalles necesarios para volver a construirla.

## Fuerzas que compiten

- Reutilizar una configuración existente evita repetir lógica de construcción.
- La copia debe conservar invariantes sin compartir accidentalmente estado mutable que debería ser independiente.
- El cliente debería pedir una copia al prototipo sin necesitar conocer su tipo concreto.
- Copiar grafos complejos puede ser más difícil de entender que construir explícitamente un objeto nuevo.

## La solución

Dar al objeto prototipo una operación de clonación —o usar el mecanismo idiomático equivalente del lenguaje— que produzca una nueva instancia con el mismo estado relevante. El cliente conserva una referencia al prototipo, solicita una copia y modifica sólo las diferencias necesarias.

El punto central no es llamar a una API llamada `clone`; es **usar una instancia existente como fuente de creación** y definir correctamente qué partes de su estado deben copiarse superficial o profundamente.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Prototype` | Expone o representa la operación de clonación. |
| `ConcretePrototype` | Define qué estado se copia y cómo se preserva la independencia necesaria. |
| Cliente | Solicita una copia al prototipo y configura únicamente las diferencias. |
| Estado mutable anidado | Debe copiarse con la profundidad apropiada para evitar aliasing accidental. |

## Cómo funciona

1. Se prepara una instancia con una configuración útil y coherente.
2. El cliente solicita una copia mediante el contrato de Prototype o el mecanismo nativo equivalente.
3. La copia conserva el estado base, pero obtiene identidad independiente.
4. El cliente modifica la copia sin alterar el prototipo cuando ese estado debe ser independiente.

## Diagrama

```mermaid
sequenceDiagram
    actor Client
    participant Prototype
    participant Clone

    Client->>Prototype: clone()
    Prototype-->>Client: nueva copia
    Client->>Clone: modifica diferencias
    Client->>Prototype: observa estado original
    Prototype-->>Client: sin cambios
```

El comportamiento importante del diagrama es la independencia: la copia parte del mismo estado configurado, pero una mutación posterior de la copia no debe contaminar al prototipo cuando el contrato promete separación.

## Ejemplo mínimo

```text
prototype = ServiceProfile("orders", ["metrics"])
canary = prototype.clone()
canary.name = "orders-canary"
canary.features.add("tracing")

prototype -> orders: metrics
canary    -> orders-canary: metrics,tracing
```

La colección `features` debe ser independiente. Una copia superficial que haga aparecer `tracing` también en `prototype` sería un defecto observable.

## Aplicación real

### Plantillas de configuración preparadas

Un generador puede conservar perfiles previamente configurados para diferentes tipos de servicio y clonarlos antes de aplicar personalizaciones específicas de una nueva instancia. Esto evita reconstruir defaults y opciones compartidas en cada creación.

Si el objeto se construye con dos parámetros triviales y no existe estado costoso o configuración que reutilizar, un constructor o factory simple es más claro.

## En Genkidama

La filosofía de Genkidama reconoce Prototype como patrón que puede colaborar con fábricas y generación configurable, pero esta entrega no declara todavía un uso productivo deliberado concreto sin evidencia directa. No se modificará arquitectura productiva para fabricar uno artificialmente.

## Cuándo usarlo

- Crear una instancia desde cero requiere repetir una configuración extensa o costosa.
- El cliente conoce el contrato del objeto pero no debería depender de su clase concreta para copiarlo.
- Existen plantillas o configuraciones base que luego reciben pequeñas variaciones.
- La semántica de copia puede definirse claramente, incluida la profundidad necesaria para estado mutable.

## Cuándo no usarlo

- Construir el objeto es simple, explícito y barato.
- La semántica de copia de un grafo complejo es ambigua o peligrosa.
- La identidad externa del objeto no puede duplicarse con seguridad —por ejemplo handles exclusivos, conexiones activas o recursos no clonables—.
- La presión real es ensamblar paso a paso un objeto complejo: considera [Builder](Builder.md).

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Reutiliza configuraciones ya preparadas. | Obliga a definir con precisión shallow copy vs deep copy. |
| Reduce dependencia del cliente respecto de constructores concretos. | Clonar grafos con ciclos o recursos externos puede ser complejo. |
| Permite crear variantes modificando sólo diferencias. | Copias profundas pueden tener costo significativo. |
| Puede simplificar registros de plantillas configurables. | Una API de clonación mal definida puede ocultar aliasing y bugs de identidad. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [Abstract Factory](AbstractFactory.md) | often implemented with | Una fábrica puede producir variantes clonando prototipos registrados en vez de construir cada producto desde cero. |
| [Builder](Builder.md) | alternative to | Builder reconstruye progresivamente; Prototype reutiliza una instancia ya configurada. |
| [Factory Method](FactoryMethod.md) | alternative to | Factory Method delega creación mediante un hook; Prototype delega creación a la copia de una instancia. |
| [Singleton](Singleton.md) | often confused with | Singleton restringe cuántas instancias existen; Prototype facilita crear nuevas instancias a partir de otras. |

## Errores comunes y confusiones

### Confundir una copia superficial accidental con Prototype correcto

Que el lenguaje pueda duplicar una referencia o estructura no significa que la semántica sea correcta. Si el contrato necesita independencia, las colecciones u objetos mutables anidados deben copiarse con la profundidad necesaria.

### Usar clonación para evitar diseñar constructores claros

Prototype resuelve presión de reutilización de una instancia configurada. No es una excusa para ocultar invariantes o abandonar una API de construcción que sería más fácil de entender.

### Copiar identidad externa

Duplicar IDs únicos, sockets, handles, locks o conexiones puede producir dos objetos que aparentan ser independientes pero compiten por un recurso que no lo es. La operación de clonación debe definir explícitamente qué se copia y qué se reinicializa.

## Cómo comprobar una implementación

- El cliente puede obtener una nueva instancia a partir de un prototipo existente sin reconstruir toda su configuración.
- La copia conserva el estado base esperado.
- Modificar la copia no altera el prototipo cuando el contrato exige independencia.
- El ejemplo protege al menos un caso de estado mutable anidado para detectar shallow-copy accidental.
- El mecanismo usado es idiomático para el lenguaje; no se exige una jerarquía OO artificial.

## Implementaciones por lenguaje

La fuente de targets es [`learn/_meta/catalog.yml`](../learn/_meta/catalog.yml): 45 lenguajes v1 y 6 adicionales. La clasificación mantiene 48 `Applicable` y 3 `N/A`. Hay **48 ejemplos materializados y 48 verificados**.

| Lenguaje | Aplicabilidad | Ejemplo verificado | Validación | Nota |
|---|---|---|---|---|
| C# | Applicable | [`PrototypeExample.cs`](../src/Enterprise/C%23/PrototypeExample.cs) | ✅ .NET 10 compile/run | Contrato explícito + copia profunda de colección. |
| TypeScript | Applicable | [`prototype.ts`](../src/Web/TypeScriptTS/prototype.ts) | ✅ TypeScript 6 strict + Node | Contrato genérico + copia independiente del array. |
| Ada | Applicable | [`prototype.adb`](../src/Historical/Ada/prototype.adb) | ✅ GNAT 2022 warnings-as-errors compile/run | Record con copia independiente. |
| Solidity | Applicable | [`Prototype.sol`](../src/Niche/Solidity/Prototype.sol) | ✅ solc 0.8.30 + source contract | Copia explícita de struct/array dentro del contrato. |
| Fortran | Applicable | [`prototype.f90`](../src/Historical/Fortran/prototype.f90) | ✅ gfortran F2018 warnings-as-errors compile/run | Derived type con allocatable independiente. |
| Pascal | Applicable | [`prototype.pas`](../src/Historical/Pascal/prototype.pas) | ✅ FPC strict compile/run | Record con copia explícita del array dinámico. |
| Python | Applicable | [`prototype.py`](../src/Scripting/PythonPY/prototype.py) | ✅ Python compile/run | `deepcopy` encapsulado por el prototipo. |
| Visual Basic .NET | Applicable | [`PrototypeExample.vb`](../src/Enterprise/VisualBasic/PrototypeExample.vb) | ✅ .NET 10 VB compile/run | Nueva instancia + nueva `List(Of String)`. |
| C++ | Applicable | [`prototype.cpp`](../src/Systems/C%2B%2B/prototype.cpp) | ✅ C++20 warnings-as-errors | `clone()` virtual + `unique_ptr`; vector copiado por valor. |
| Objective-C | Applicable | [`prototype.m`](../src/Systems/Objective-C/prototype.m) | ✅ Clang/ARC/Foundation | `NSCopying` con colección independiente. |
| Java | Applicable | [`PrototypeExample.java`](../src/Enterprise/Java/PrototypeExample.java) | ✅ Java 25 `-Werror` | Contrato `Prototype<T>` + nueva colección. |
| Rust | Applicable | [`prototype.rs`](../src/Systems/Rust/prototype.rs) | ✅ rustfmt + `rustc -D warnings` | `Clone` idiomático duplica `Vec<String>`. |
| Zig | Applicable | [`prototype.zig`](../src/Systems/Zig/prototype.zig) | ✅ Zig 0.16 fmt + run | Struct por valor con buffers propios. |
| Go | Applicable | [`prototype.go`](../src/Systems/Go/prototype.go) | ✅ gofmt + vet + run | Método `Clone` duplica el slice mutable. |
| PHP | Applicable | [`prototype.php`](../src/Scripting/PHP/prototype.php) | ✅ lint/run | `clone`; arrays usan copy-on-write y divergen al modificar la copia. |
| Nim | Applicable | [`prototype.nim`](../src/Niche/Nim/prototype.nim) | ✅ Nim compile/run | Object con nueva secuencia de features. |
| Dart | Applicable | [`prototype.dart`](../src/Web/Dart/prototype.dart) | ✅ Dart format + analyze + run | Copia explícita de lista. |
| Kotlin | Applicable | [`PrototypeExample.kt`](../src/Enterprise/Kotlin/PrototypeExample.kt) | ✅ kotlinc/JVM | `copy` + `toMutableList` evita compartir estado mutable. |
| Swift | Applicable | [`prototype.swift`](../src/Systems/Swift/prototype.swift) | ✅ swiftc compile/run | Value semantics + copy-on-write de `Array`. |
| F# | Applicable | [`prototype.fsx`](../src/Functional/F%23/prototype.fsx) | ✅ `dotnet fsi` | Record + lista inmutable para derivar la variante. |
| Crystal | Applicable | [`prototype.cr`](../src/Niche/Crystal/prototype.cr) | ✅ format + compile/run | Objeto con duplicación explícita del array. |
| Lua | Applicable | [`prototype.lua`](../src/Scripting/Lua/prototype.lua) | ✅ Lua 5.4 run | Table clone con nuevo array de features. |
| Haskell | Applicable | [`Prototype.hs`](../src/Functional/Haskell/Prototype.hs) | ✅ GHC warnings-as-errors | Valor inmutable derivado desde la plantilla. |
| COBOL | Applicable | [`prototype.cbl`](../src/Historical/Cobol/prototype.cbl) | ✅ GnuCOBOL compile/run | Record copiado y especializado de forma independiente. |
| Scala | Applicable | [`Prototype.scala`](../src/Functional/Scala/Prototype.scala) | ✅ scalac/run | Case class + `copy` sobre `Vector` inmutable. |
| Groovy | Applicable | [`prototype.groovy`](../src/Scripting/Groovy/prototype.groovy) | ✅ Groovy compile/run | Objeto copiado con colección independiente. |
| Ruby | Applicable | [`prototype.rb`](../src/Scripting/RubyRB/prototype.rb) | ✅ syntax/run | `dup` + `initialize_copy` para duplicar estado mutable. |
| C | Applicable | [`prototype.c`](../src/Systems/C/prototype.c) | ✅ C17 warnings-as-errors | Struct por valor con buffers propios. |
| OCaml | Applicable | [`prototype.ml`](../src/Functional/OCaml/prototype.ml) | ✅ ocamlc warnings-as-errors | Record derivado explícitamente desde la plantilla. |
| Julia | Applicable | [`prototype.jl`](../src/DataScience/Julia/prototype.jl) | ✅ Julia run | Copia/derivación con estado independiente. |
| VBA | Applicable | [`prototype.bas`](../src/Shell/VBA/prototype.bas) | ✅ source contract sobre VBA real | Class module clona y duplica la colección; Office/VBA no está disponible en hosted CI. |
| GDScript | Applicable | [`prototype.gd`](../src/Niche/GDScript/prototype.gd) | ✅ Godot 4.6.3 headless | Duplicación profunda y especialización observable. |
| JavaScript | Applicable | [`prototype.js`](../src/Web/JavaScriptJS/prototype.js) | ✅ Node syntax/run | Prototipo nativo + copia independiente de array mutable. |
| MATLAB | Applicable | [`prototype.m`](../src/DataScience/MATLAB/prototype.m) | ✅ MathWorks Actions | Valor copiado y especializado sin alterar el original. |
| Perl | Applicable | [`prototype.pl`](../src/Scripting/Perl/prototype.pl) | ✅ syntax/run | Hash + nuevo array para el clon. |
| R | Applicable | [`prototype.R`](../src/DataScience/R/prototype.R) | ✅ Rscript | Lista derivada con vector independiente. |
| PowerShell | Applicable | [`prototype.ps1`](../src/Shell/PowerShell/prototype.ps1) | ✅ pwsh StrictMode/run | PSCustomObject + nueva lista genérica. |
| HTML | N/A | — | — | HTML describe markup; una rutina de clonación ejecutable pertenece al runtime que la implementa. |
| Assembly | Applicable | [`prototype.asm`](../src/LowLevel/Assembly/prototype.asm) | ✅ NASM/ld compile/run | Copia explícita de buffers. |
| Elixir | Applicable | [`prototype.exs`](../src/Functional/Elixir/prototype.exs) | ✅ Elixir compile/run | Datos inmutables derivados desde la plantilla. |
| Shell | Applicable | [`prototype.sh`](../src/Shell/Bash/prototype.sh) | ✅ bash syntax/run | Arrays copiados mediante referencias por nombre, sin compartir estado. |
| Erlang | Applicable | [`prototype.erl`](../src/Functional/Erlang/prototype.erl) | ✅ Erlang compile/run | Map inmutable derivado desde el prototipo. |
| Clojure | Applicable | [`prototype.clj`](../src/Functional/Clojure/prototype.clj) | ✅ Clojure run | Persistent data structure derivada desde la plantilla. |
| Common Lisp | Applicable | [`prototype.lisp`](../src/Functional/Lisp/prototype.lisp) | ✅ SBCL run | Estructura/lista copiada; helper evita colisión con `COMMON-LISP:DESCRIBE`. |
| Prolog | Applicable | [`prototype.pl`](../src/Niche/Prolog/prototype.pl) | ✅ SWI-Prolog | Término base copiado y especializado declarativamente. |
| Delphi | Applicable | [`Prototype.pas`](../src/Enterprise/Delphi/Prototype.pas) | ✅ source contract sobre Delphi real | `Clone` duplica `TStringList`; DCC propietario no está disponible en hosted CI. |
| GNU Octave | Applicable | [`prototype.m`](../src/DataScience/Octave/prototype.m) | ✅ Octave run | Struct copiado y especializado. |
| SQL | N/A | — | — | SQL declarativo puede duplicar datos, pero no expresa por sí mismo un objeto runtime responsable de clonarse dentro de este catálogo. |
| CSS | N/A | — | — | CSS define reglas de presentación; no expresa una operación runtime de clonación de objetos. |
| MicroPython | Applicable | [`prototype.py`](../src/Other/MicroPython/prototype.py) | ✅ MicroPython 1.28.0 Unix port | Copia explícita ejecutada en el Unix port oficial. |
| Rockstar | Applicable | [`prototype.rock`](../src/Other/Rockstar/prototype.rock) | ✅ Rockstar v2.0.31 runtime | Keyed arrays copiados campo a campo con el runtime oficial fijado por SHA-256. |

## Comprueba que lo entendiste

1. Si una copia comparte accidentalmente una lista mutable con el prototipo, ¿qué comportamiento observarías y qué decisión de copia falta?
2. ¿Cuándo elegirías Builder en lugar de Prototype para crear una variante?
3. ¿Qué tipos de recursos no deberían copiarse ciegamente aunque el resto del objeto sí pueda clonarse?

## Resumen

- La presión central es reutilizar una **instancia ya configurada** como fuente de creación.
- El movimiento de diseño es clonar el prototipo y modificar sólo las diferencias.
- El principal trade-off es definir correctamente identidad y profundidad de copia.
- Builder reconstruye paso a paso; Prototype reutiliza estado ya preparado.
- El patrón puede expresarse sin clases mediante records, structs, maps, funciones y valores inmutables.

## Referencias

- Gamma, Helm, Johnson y Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software* — Prototype.
- [Patterns as Living Examples](../docs/philosophy/001-patterns-as-living-examples.md).
- [Canonical Design Pattern Authoring Standard](../docs/kb/catalog/pattern-authoring-standard.md).
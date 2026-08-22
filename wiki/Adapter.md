# Adapter

> **Familia:** Structural  
> **Intención:** convertir la interfaz de una dependencia existente en el contrato que un cliente necesita, sin modificar ni al cliente ni a la dependencia adaptada.  
> **Estado:** `validated`  
> **Implementaciones de lenguaje:** `48/48`  
> **Cobertura de pruebas:** N/A — el catálogo usa evidencia proporcional por ecosistema; no existe una métrica homogénea entre ejemplos standalone.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Adapter permite que dos piezas con contratos incompatibles colaboren mediante una capa que traduce interfaz, datos o convenciones sin contaminar a ninguna de las dos.

## El problema

Una aplicación ya habla un contrato útil para su dominio, pero una biblioteca, servicio externo, API heredada o proveedor ofrece otra interfaz: nombres distintos, unidades diferentes, estructuras incompatibles o convenciones que no queremos propagar al resto del sistema. Reescribir la dependencia puede ser imposible y cambiar todos los clientes trasladaría el acoplamiento externo hacia dentro.

## Fuerzas que compiten

- El cliente necesita un contrato estable y expresivo para su dominio.
- La dependencia existente no puede o no conviene modificarse.
- La traducción debe quedar localizada para que cambios del proveedor no se filtren por toda la aplicación.
- Una capa adicional cuesta código y puede ocultar capacidades específicas de la dependencia.

## La solución

Introducir un **Adapter** que implemente el contrato esperado por el cliente, mantenga o reciba una referencia al **Adaptee** y traduzca cada operación hacia su interfaz real. La traducción puede incluir nombres, tipos, estructuras, unidades, códigos de error o protocolos, pero debe preservar la semántica que el cliente necesita.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Client` | Trabaja únicamente con el contrato objetivo. |
| `Target` | Define la interfaz que el cliente entiende y necesita. |
| `Adapter` | Implementa `Target` y traduce llamadas/datos al contrato del adaptee. |
| `Adaptee` | Dependencia existente con una interfaz incompatible pero funcionalidad aprovechable. |

## Cómo funciona

1. El cliente invoca una operación del contrato `Target`.
2. El Adapter traduce esa operación al protocolo del Adaptee.
3. El Adaptee ejecuta su comportamiento normal.
4. El Adapter convierte el resultado o error al contrato esperado por el cliente.

## Diagrama

```mermaid
sequenceDiagram
    actor Client
    participant Target as TemperatureReader
    participant Adapter as FahrenheitSensorAdapter
    participant Adaptee as LegacyFahrenheitSensor

    Client->>Target: readCelsius()
    Target->>Adapter: readCelsius()
    Adapter->>Adaptee: readFahrenheit()
    Adaptee-->>Adapter: 86°F
    Adapter-->>Client: 30°C
```

El cliente nunca aprende el método `readFahrenheit()` ni la unidad heredada; esas decisiones quedan detrás del Adapter.

## Ejemplo mínimo

```text
legacy = LegacyFahrenheitSensor()      // sólo sabe devolver Fahrenheit
reader = FahrenheitSensorAdapter(legacy)
print(reader.readCelsius())            // 30
```

El Adapter cambia tanto la **interfaz** (`readFahrenheit` → `readCelsius`) como la representación de datos (°F → °C), mientras el sensor heredado permanece intacto.

## Aplicación real

### Integrar un proveedor con contrato ajeno

Una aplicación puede esperar `PaymentGateway.charge(cents)` mientras un proveedor heredado expone otro método, otras unidades monetarias y códigos de resultado propios. Un Adapter mantiene el modelo interno estable y concentra esa traducción en una frontera explícita.

Si la dependencia ya satisface el contrato útil del dominio, una capa Adapter no aporta valor y sólo añade indirección.

## En Genkidama

La filosofía del repositorio identifica **integraciones externas y clientes específicos de proveedor** como un lugar natural para Adapter. Sin embargo, esta auditoría no ha confirmado todavía un uso productivo deliberado que deba presentarse como ejemplo canónico. Por ello no se modificará arquitectura de producción para fabricar uno.

## Cuándo usarlo

- Debes reutilizar una biblioteca, API o componente cuya interfaz no coincide con la que necesita tu cliente.
- Quieres aislar convenciones externas —tipos, unidades, códigos o protocolos— detrás de un contrato propio.
- Varias implementaciones externas deben verse iguales desde el núcleo de la aplicación.

## Cuándo no usarlo

- El proveedor ya implementa directamente el contrato que necesitas.
- Sólo estás renombrando una función sin reducir acoplamiento ni traducir semántica.
- Necesitas simplificar una fachada completa de subsistemas: considera Facade.
- Necesitas variar el comportamiento intercambiable bajo el mismo contrato, no convertir contratos incompatibles: considera Strategy.

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Aísla dependencias y convenciones externas. | Añade una capa y más código de integración. |
| Mantiene estable al cliente frente a cambios del proveedor. | Puede esconder capacidades específicas que algunos clientes sí necesitan. |
| Facilita sustituir o probar proveedores mediante el contrato objetivo. | Una traducción incorrecta puede alterar semántica, unidades o errores. |
| Evita contaminar el dominio con modelos ajenos. | Demasiados adapters triviales pueden fragmentar innecesariamente el diseño. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [Facade](Facade.md) | often confused with | Facade simplifica el acceso a un subsistema; Adapter hace compatible un contrato con otro. |
| [Bridge](Bridge.md) | often confused with | Bridge separa abstracción e implementación deliberadamente desde el diseño; Adapter reconcilia interfaces ya incompatibles. |
| [Proxy](Proxy.md) | often confused with | Proxy conserva esencialmente el mismo contrato para controlar acceso; Adapter cambia el contrato visto por el cliente. |
| [Strategy](Strategy.md) | collaborates with | Varios adapters pueden quedar detrás de una Strategy o contrato común cuando además hay selección de comportamiento. |

## Errores comunes y confusiones

### Llamar Adapter a cualquier wrapper

Un wrapper no es Adapter sólo porque delega. Debe existir una incompatibilidad concreta que se traduce para permitir colaboración bajo el contrato objetivo.

### Filtrar el modelo del proveedor hacia el cliente

Si el cliente sigue manejando tipos, unidades o códigos del Adaptee, el acoplamiento no quedó realmente aislado. La traducción debe ocurrir en la frontera.

### Convertir Adapter en lógica de negocio

El Adapter traduce contratos. Reglas de negocio independientes del proveedor pertenecen al dominio o a servicios apropiados, no a la capa de adaptación.

## Cómo comprobar una implementación

- El cliente sólo depende del contrato `Target`; no invoca directamente la interfaz incompatible.
- El Adaptee puede conservar su API original sin cambios.
- La misma lectura heredada de `86°F` se observa como `30°C` a través del Adapter.
- Una traducción equivocada de interfaz o unidades hace fallar la prueba observable.

## Implementaciones por lenguaje

La fuente canónica mantiene **51 targets**. Adapter es `Applicable` en 48 y `N/A` en HTML, CSS y SQL declarativo. Los **48/48 ejemplos Applicable están materializados, enlazados y verificados** mediante CI ejecutable o evidencia proporcional de plataforma.

| Lenguaje | Aplicabilidad | Ejemplo verificado | Validación | Nota |
|---|---|---|---|---|
| C# | Applicable | [`AdapterExample.cs`](../src/Enterprise/C%23/AdapterExample.cs) | ✅ .NET 10 compile/run + contrato observable | Interfaz objetivo + composición del adaptee. |
| TypeScript | Applicable | [`adapter.ts`](../src/Web/TypeScriptTS/adapter.ts) | ✅ TypeScript 6 strict + Node | Interface estructural + clase adapter. |
| Ada | Applicable | [`adapter.adb`](../src/Historical/Ada/adapter.adb) | ✅ GNAT 2022 warnings-as-errors + run | Función Target traduce la operación heredada sin requerir clases. |
| Solidity | Applicable | [`Adapter.sol`](../src/Niche/Solidity/Adapter.sol) | ✅ solc 0.8.30 compile/source contract | Contrato wrapper implementa Target y compone al Adaptee. |
| Fortran | Applicable | [`adapter.f90`](../src/Historical/Fortran/adapter.f90) | ✅ gfortran F2018 warnings-as-errors + run | Derived types/procedure bindings separan Target y Adaptee. |
| Pascal | Applicable | [`adapter.pas`](../src/Historical/Pascal/adapter.pas) | ✅ Free Pascal `-Sew` + run | Clase Target abstracta + composición. |
| Python | Applicable | [`adapter.py`](../src/Scripting/PythonPY/adapter.py) | ✅ `py_compile` + Python 3.14 run | Duck typing mantiene el Target liviano. |
| Visual Basic .NET | Applicable | [`AdapterExample.vb`](../src/Enterprise/VisualBasic/AdapterExample.vb) | ✅ .NET 10 VB compile/run | Interface y composición. |
| C++ | Applicable | [`adapter.cpp`](../src/Systems/C%2B%2B/adapter.cpp) | ✅ C++20 warnings-as-errors + run | Interfaz abstracta + composición. |
| Objective-C | Applicable | [`adapter.m`](../src/Systems/Objective-C/adapter.m) | ✅ macOS Clang/ARC/Foundation + run | Protocol Target + wrapper sobre objeto heredado. |
| Java | Applicable | [`AdapterExample.java`](../src/Enterprise/Java/AdapterExample.java) | ✅ Java 25 `-Xlint:all -Werror` + run | Interface Target + composición. |
| Rust | Applicable | [`adapter.rs`](../src/Systems/Rust/adapter.rs) | ✅ rustfmt + `rustc -D warnings` + run | Trait Target + struct adapter. |
| Zig | Applicable | [`adapter.zig`](../src/Systems/Zig/adapter.zig) | ✅ Zig fmt/check + run | Struct Adapter compone al sensor y traduce la unidad. |
| Go | Applicable | [`adapter.go`](../src/Systems/Go/adapter.go) | ✅ gofmt + go vet + run | Interface implícita + composición. |
| PHP | Applicable | [`adapter.php`](../src/Scripting/PHP/adapter.php) | ✅ PHP lint + run | Interface + wrapper. |
| Nim | Applicable | [`adapter.nim`](../src/Niche/Nim/adapter.nim) | ✅ Nim warnings + compile/run | Object + proc Target sobre un Adaptee compuesto. |
| Dart | Applicable | [`adapter.dart`](../src/Web/Dart/adapter.dart) | ✅ Dart format/analyze + run | Interface explícita + composición. |
| Kotlin | Applicable | [`AdapterExample.kt`](../src/Enterprise/Kotlin/AdapterExample.kt) | ✅ kotlinc + JVM run | Interface + composición. |
| Swift | Applicable | [`adapter.swift`](../src/Systems/Swift/adapter.swift) | ✅ swiftc + run | Protocol + wrapper. |
| F# | Applicable | [`adapter.fsx`](../src/Functional/F%23/adapter.fsx) | ✅ `dotnet fsi` + contrato observable | Interface o funciones adaptadoras idiomáticas. |
| Crystal | Applicable | [`adapter.cr`](../src/Niche/Crystal/adapter.cr) | ✅ Crystal format/build + run | Clase Adapter satisface Target y compone al legacy sensor. |
| Lua | Applicable | [`adapter.lua`](../src/Scripting/Lua/adapter.lua) | ✅ Lua 5.4 run | Table + closure traducen operación/unidad. |
| Haskell | Applicable | [`Adapter.hs`](../src/Functional/Haskell/Adapter.hs) | ✅ GHC warnings-as-errors + run | Función adaptadora devuelve contrato objetivo. |
| COBOL | Applicable | [`adapter.cbl`](../src/Historical/Cobol/adapter.cbl) | ✅ GnuCOBOL compile/run | Paragraph wrapper traduce operación y representación. |
| Scala | Applicable | [`Adapter.scala`](../src/Functional/Scala/Adapter.scala) | ✅ scalac + run | Trait + wrapper. |
| Groovy | Applicable | [`adapter.groovy`](../src/Scripting/Groovy/adapter.groovy) | ✅ Groovy compile/run | Dynamic dispatch + wrapper. |
| Ruby | Applicable | [`adapter.rb`](../src/Scripting/RubyRB/adapter.rb) | ✅ Ruby syntax + run | Duck typing + wrapper. |
| C | Applicable | [`adapter.c`](../src/Systems/C/adapter.c) | ✅ GCC C17 warnings-as-errors + run | Structs + function pointers traducen el contrato. |
| OCaml | Applicable | [`adapter.ml`](../src/Functional/OCaml/adapter.ml) | ✅ OCaml warnings-as-errors + run | Función de orden superior transforma el contrato. |
| Julia | Applicable | [`adapter.jl`](../src/DataScience/Julia/adapter.jl) | ✅ Julia runtime + contrato observable | Función adaptadora devuelve una nueva operación Celsius. |
| VBA | Applicable | [`adapter.bas`](../src/Shell/VBA/adapter.bas) + [classes](../src/Shell/VBA/FahrenheitSensorAdapter.cls) | ✅ source-contract sobre VBA real | `Implements` + class modules preservan Target/Adaptee reales. |
| GDScript | Applicable | [`adapter.gd`](../src/Niche/GDScript/adapter.gd) | ✅ Godot 4.6.3 headless + run | Script wrapper traduce método y unidad. |
| JavaScript | Applicable | [`adapter.js`](../src/Web/JavaScriptJS/adapter.js) | ✅ Node syntax + run | Duck typing + class adapter. |
| MATLAB | Applicable | [`adapter.m`](../src/DataScience/MATLAB/adapter.m) | ✅ MathWorks Actions + run | Function handles traducen la dependencia sin ceremonia OO. |
| Perl | Applicable | [`adapter.pl`](../src/Scripting/Perl/adapter.pl) | ✅ Perl syntax + run | Packages traducen el contrato. |
| R | Applicable | [`adapter.R`](../src/DataScience/R/adapter.R) | ✅ Rscript + contrato observable | Closure adapta una operación Fahrenheit a Celsius. |
| PowerShell | Applicable | [`adapter.ps1`](../src/Shell/PowerShell/adapter.ps1) | ✅ PowerShell strict-mode run | Script object + closure mantienen al adaptee detrás de Target. |
| HTML | N/A | — | — | Markup declarativo; la adaptación ejecutable pertenece al runtime que lo procesa. |
| Assembly | Applicable | [`adapter.asm`](../src/LowLevel/Assembly/adapter.asm) | ✅ NASM/ld + run + traducción 86→30 | Wrapper routine traduce operación y verifica 86→30 antes de emitir. |
| Elixir | Applicable | [`adapter.exs`](../src/Functional/Elixir/adapter.exs) | ✅ Elixir warnings-as-errors + run | Módulo Adapter traduce la llamada al módulo heredado. |
| Shell | Applicable | [`adapter.sh`](../src/Shell/Bash/adapter.sh) | ✅ Bash syntax + run | Functions normalizan operación y unidad. |
| Erlang | Applicable | [`adapter.erl`](../src/Functional/Erlang/adapter.erl) | ✅ Erlang warnings-as-errors + run | Higher-order function traduce el protocolo de lectura. |
| Clojure | Applicable | [`adapter.clj`](../src/Functional/Clojure/adapter.clj) | ✅ Clojure runtime + contrato observable | Closure adapta el contrato sin forma OO. |
| Common Lisp | Applicable | [`adapter.lisp`](../src/Functional/Lisp/adapter.lisp) | ✅ SBCL non-interactive + contrato observable | Closure captura la operación incompatible. |
| Prolog | Applicable | [`adapter.pl`](../src/Niche/Prolog/adapter.pl) | ✅ SWI-Prolog run | Predicado wrapper traduce valor y unidad. |
| Delphi | Applicable | [`AdapterExample.pas`](../src/Enterprise/Delphi/AdapterExample.pas) | ✅ source-contract sobre Delphi real | Interface + composición sobre el contrato real de Delphi. |
| GNU Octave | Applicable | [`adapter.m`](../src/DataScience/Octave/adapter.m) | ✅ Octave headless + contrato observable | Function handle traduce el contrato. |
| SQL | N/A | — | — | SQL declarativo transforma datos, pero no expresa por sí mismo una frontera de objeto/componente con contrato Target/Adaptee. |
| CSS | N/A | — | — | Reglas declarativas de presentación sin llamadas de componente a adaptar. |
| MicroPython | Applicable | [`adapter.py`](../src/Other/MicroPython/adapter.py) | ✅ MicroPython 1.28.0 Unix port oficial + run | Duck typing/clases livianas bajo el runtime MicroPython real. |
| Rockstar | Applicable | [`adapter.rock`](../src/Other/Rockstar/adapter.rock) | ✅ Rockstar v2.0.31 oficial + salida exacta | Keyed array + función Adapter traducen la representación. |

## Comprueba que lo entendiste

1. Si una API externa ya devuelve exactamente el contrato que tu dominio necesita, ¿qué presión justificaría todavía un Adapter?
2. ¿Por qué Facade y Adapter pueden envolver el mismo proveedor pero resolver problemas distintos?
3. ¿Qué evidencia demostraría que un Adapter realmente aisló unidades y no sólo renombró un método?

## Resumen

- **Presión:** cliente y dependencia útil hablan contratos incompatibles.
- **Movimiento:** una frontera traduce del Target hacia el Adaptee y vuelve al lenguaje del cliente.
- **Trade-off:** aislamiento y sustituibilidad a cambio de otra capa de integración.
- **Relaciones:** se confunde con Facade, Bridge y Proxy, pero cambia el contrato por una razón distinta.
- **Portabilidad:** no requiere clases; cualquier mecanismo que traduzca interfaz y semántica puede expresar Adapter.

## Referencias

- Gamma, Helm, Johnson y Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software* — Adapter.
- [Patterns as Living Examples](../docs/philosophy/001-patterns-as-living-examples.md).
- [Canonical Design Pattern Authoring Standard](../docs/kb/catalog/pattern-authoring-standard.md).
# Singleton

> **Familia:** Creational  
> **Intención:** asegurar una única instancia lógica dentro de un alcance definido y ofrecer un punto de acceso controlado cuando esa unicidad resuelve una presión real.  
> **Estado:** `validated`  
> **Implementaciones de lenguaje:** `48/48`  
> **Cobertura de pruebas:** N/A — catálogo multilenguaje heterogéneo; se usa evidencia proporcional por target.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Singleton concentra una responsabilidad que debe tener **una sola instancia lógica por alcance** y controla cómo se obtiene esa instancia.

## El problema

Algunos recursos coordinadores —por ejemplo un registro de configuración de proceso, un reloj de aplicación o un catálogo de metadatos inmutable— necesitan una única autoridad lógica. Si cada consumidor crea su propia copia, el estado puede divergir; si se usa una variable global sin disciplina, cualquier código puede reemplazarla o mutarla sin contrato.

La presión real no es “quiero acceso global”, sino “necesito exactamente una autoridad compartida y debo controlar su ciclo de vida”.

## Fuerzas que compiten

- Debe existir una sola instancia lógica dentro de un alcance definido.
- Los consumidores necesitan acceso consistente sin conocer cómo se crea la instancia.
- La inicialización debe ser segura y determinista, incluso bajo concurrencia cuando aplique.
- El acceso global aumenta acoplamiento oculto y dificulta aislamiento de pruebas.
- En sistemas distribuidos, “único en el proceso” no significa “único en todo el sistema”.

## La solución

Encapsular la creación y exponer un único valor compartido mediante el mecanismo idiomático del lenguaje: inicialización estática, módulo, objeto global inmutable, lazy cell, función con caché, proceso registrado, símbolo o equivalente. El constructor o mecanismo de creación queda fuera del acceso normal cuando el lenguaje lo permite.

La unicidad siempre debe declarar su **alcance**: proceso, módulo, runtime, actor system, request scope u otro límite concreto.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Singleton` | Mantiene o representa la única instancia lógica. |
| Accesor | Devuelve siempre esa instancia dentro del alcance definido. |
| Cliente | Consume la instancia sin crear copias arbitrarias. |

## Cómo funciona

1. El runtime o el primer acceso inicializa la instancia compartida.
2. El accesor devuelve la misma instancia en accesos posteriores.
3. Los clientes observan el mismo estado/identidad dentro del alcance declarado.
4. El ciclo de vida termina con el alcance; no se infiere unicidad distribuida.

## Diagrama

```mermaid
sequenceDiagram
    actor ClientA
    actor ClientB
    participant Accessor
    participant Singleton
    ClientA->>Accessor: instance()
    Accessor-->>ClientA: shared instance
    ClientB->>Accessor: instance()
    Accessor-->>ClientB: same shared instance
    ClientA->>Singleton: increment()
    ClientB->>Singleton: read()
    Singleton-->>ClientB: updated shared state
```

El diagrama enfatiza dos propiedades: ambos clientes reciben la misma autoridad lógica y los cambios observables pertenecen a esa única instancia.

## Ejemplo mínimo

```text
first = Registry.instance()
second = Registry.instance()
first.increment()
assert same(first, second)
assert second.count == 1
```

## Aplicación real

### Registro de configuración de proceso

Un proceso necesita una única fuente en memoria para configuración ya validada. Singleton puede ser razonable si el alcance es realmente el proceso y la instancia no necesita variar por tenant, request o prueba.

Si la dependencia debe poder sustituirse, configurarse por alcance o aislarse en tests, Dependency Injection suele ser una opción mejor.

## En Genkidama

Genkidama no declara actualmente un uso deliberado de Singleton que deba promocionarse como referencia canónica. El catálogo no modificará arquitectura productiva para fabricar uno.

## Cuándo usarlo

- Existe una restricción real de una única autoridad lógica por alcance.
- El ciclo de vida está claramente definido y coincide con el alcance de la aplicación.
- La creación repetida produciría divergencia o conflicto observable.

## Cuándo no usarlo

- Sólo quieres evitar pasar una dependencia explícitamente: usa Dependency Injection.
- Necesitas variantes por request, tenant o prueba.
- Pretendes unicidad entre procesos o máquinas: usa coordinación distribuida, no Singleton de proceso.
- El objeto no tiene una restricción real de unicidad.

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Hace explícita una autoridad única. | Introduce dependencia global implícita si se abusa. |
| Centraliza inicialización y ciclo de vida. | Puede dificultar pruebas aisladas y sustitución. |
| Evita copias divergentes dentro del alcance. | No resuelve unicidad distribuida. |
| Puede aprovechar primitivas seguras del runtime. | Estado mutable global puede convertirse en cuello de botella. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [Dependency Injection](DependencyInjection.md) | alternative to | DI hace explícito el ciclo de vida y suele facilitar sustitución/pruebas cuando la unicidad no exige acceso global. |
| [Factory Method](FactoryMethod.md) | collaborates with | Un factory method puede controlar creación, pero no implica una única instancia. |
| [Abstract Factory](AbstractFactory.md) | often confused with | Abstract Factory selecciona familias; Singleton restringe cantidad/ciclo de vida. |
| [Object Pool](ObjectPool.md) | alternative to | Pool mantiene varias instancias reutilizables; Singleton mantiene una sola. |

## Errores comunes y confusiones

### Confundir Singleton con una variable global

Una variable global puede reasignarse o carecer de control de creación. Singleton expresa una restricción de instancia y un acceso controlado.

### Confundir “una por proceso” con “una en todo el sistema”

Dos procesos pueden tener cada uno su Singleton. La unicidad distribuida requiere mecanismos externos de coordinación.

### Usarlo como Service Locator

Acumular muchas dependencias detrás de un Singleton convierte el acceso global en un contenedor opaco y aumenta acoplamiento.

## Cómo comprobar una implementación

- Dos accesos dentro del mismo alcance obtienen la misma instancia lógica o el mismo valor compartido canónico.
- Una mutación observable realizada por un cliente es visible al otro cuando el ejemplo usa estado mutable.
- La inicialización no produce dos instancias bajo el mecanismo normal del runtime.
- La documentación declara el alcance de la unicidad.
- La prueba no se limita a nombres como `Singleton` o `Instance`.

## Implementaciones por lenguaje

La fuente canónica mantiene **51 targets**: 48 `Applicable` y 3 `N/A`. Los **48 Applicable tienen un ejemplo real, enlazado y verificado** mediante el runtime, compilador, analizador o source-contract proporcional más fuerte razonablemente disponible.

| Lenguaje | Aplicabilidad | Ejemplo | Validación actual | Nota |
|---|---|---|---|---|
| C# | Applicable | [`SingletonExample.cs`](../src/Enterprise/C%23/SingletonExample.cs) | ✅ verificado | `Lazy<T>` + constructor privado; alcance de proceso. |
| TypeScript | Applicable | [`singleton.ts`](../src/Web/TypeScriptTS/singleton.ts) | ✅ verificado | Instancia estática privada y acceso controlado. |
| Ada | Applicable | [`singleton.adb`](../src/Historical/Ada/singleton.adb) | ✅ verificado | Package state + access value canónico dentro del proceso. |
| Solidity | Applicable | [`Singleton.sol`](../src/Niche/Solidity/Singleton.sol) | ✅ verificado | La unicidad está acotada a una instancia de contrato, no a toda la blockchain. |
| Fortran | Applicable | [`singleton.f90`](../src/Historical/Fortran/singleton.f90) | ✅ verificado | Módulo con `save` + pointer a un único estado. |
| Pascal | Applicable | [`singleton.pas`](../src/Historical/Pascal/singleton.pas) | ✅ verificado | Registry process-scoped y accessor que devuelve siempre el mismo puntero. |
| Python | Applicable | [`singleton.py`](../src/Scripting/PythonPY/singleton.py) | ✅ verificado | `__new__` controla la única instancia de proceso. |
| Visual Basic .NET | Applicable | [`SingletonExample.vb`](../src/Enterprise/VisualBasic/SingletonExample.vb) | ✅ verificado | `Shared` + constructor privado. |
| C++ | Applicable | [`singleton.cpp`](../src/Systems/C%2B%2B/singleton.cpp) | ✅ verificado | Function-local static; inicialización segura desde C++11. |
| Objective-C | Applicable | [`singleton.m`](../src/Systems/Objective-C/singleton.m) | ✅ verificado | `dispatch_once` controla una única instancia de proceso. |
| Java | Applicable | [`SingletonExample.java`](../src/Enterprise/Java/SingletonExample.java) | ✅ verificado | Initialization-on-demand holder idiom. |
| Rust | Applicable | [`singleton.rs`](../src/Systems/Rust/singleton.rs) | ✅ verificado | `OnceLock<Mutex<_>>` para estado compartido de proceso. |
| Zig | Applicable | [`singleton.zig`](../src/Systems/Zig/singleton.zig) | ✅ verificado | Binding global encapsulado por accessor. |
| Go | Applicable | [`singleton.go`](../src/Systems/Go/singleton.go) | ✅ verificado | `sync.Once` + package state. |
| PHP | Applicable | [`singleton.php`](../src/Scripting/PHP/singleton.php) | ✅ verificado | Propiedad estática y constructor privado por proceso/request runtime. |
| Nim | Applicable | [`singleton.nim`](../src/Niche/Nim/singleton.nim) | ✅ verificado | Estado global y accessor a una única dirección. |
| Dart | Applicable | [`singleton.dart`](../src/Web/Dart/singleton.dart) | ✅ verificado | Factory constructor + `static final`. |
| Kotlin | Applicable | [`SingletonExample.kt`](../src/Enterprise/Kotlin/SingletonExample.kt) | ✅ verificado | `object` nativo. |
| Swift | Applicable | [`singleton.swift`](../src/Systems/Swift/singleton.swift) | ✅ verificado | `static let shared`; inicialización segura del runtime. |
| F# | Applicable | [`singleton.fsx`](../src/Functional/F%23/singleton.fsx) | ✅ verificado | Static binding privado + acceso único. |
| Crystal | Applicable | [`singleton.cr`](../src/Niche/Crystal/singleton.cr) | ✅ verificado | Class variable encapsulada y constructor privado. |
| Lua | Applicable | [`singleton.lua`](../src/Scripting/Lua/singleton.lua) | ✅ verificado | Módulo/binding devuelve la misma tabla del proceso. |
| Haskell | Applicable | [`Singleton.hs`](../src/Functional/Haskell/Singleton.hs) | ✅ verificado | Binding `NOINLINE` con `IORef` canónica de proceso. |
| COBOL | Applicable | [`singleton.cbl`](../src/Historical/Cobol/singleton.cbl) | ✅ verificado | Dos pointers apuntan al mismo working-storage registry del programa. |
| Scala | Applicable | [`Singleton.scala`](../src/Functional/Scala/Singleton.scala) | ✅ verificado | `object` nativo. |
| Groovy | Applicable | [`singleton.groovy`](../src/Scripting/Groovy/singleton.groovy) | ✅ verificado | Static holder explícito; no depende sólo de una anotación. |
| Ruby | Applicable | [`singleton.rb`](../src/Scripting/RubyRB/singleton.rb) | ✅ verificado | Módulo estándar `Singleton`; constructor controlado. |
| C | Applicable | [`singleton.c`](../src/Systems/C/singleton.c) | ✅ verificado | File-static state + accessor; alcance de proceso. |
| OCaml | Applicable | [`singleton.ml`](../src/Functional/OCaml/singleton.ml) | ✅ verificado | Module binding con `ref` compartida. |
| Julia | Applicable | [`singleton.jl`](../src/DataScience/Julia/singleton.jl) | ✅ verificado | Binding `const` con `Ref` compartida. |
| VBA | Applicable | [`singleton.bas`](../src/Shell/VBA/singleton.bas) | ✅ verificado | Módulo VBA real con estado privado y accessor; source-contract proporcional. |
| GDScript | Applicable | [`singleton.gd`](../src/Niche/GDScript/singleton.gd) | ✅ verificado | Una única instancia de script/árbol representa el alcance del ejemplo; Autoload es el mecanismo de proyecto habitual. |
| JavaScript | Applicable | [`singleton.js`](../src/Web/JavaScriptJS/singleton.js) | ✅ verificado | Constructor retorna instancia compartida. |
| MATLAB | Applicable | [`singleton.m`](../src/DataScience/MATLAB/singleton.m) | ✅ verificado | `persistent` conserva una única handle instance. |
| Perl | Applicable | [`singleton.pl`](../src/Scripting/Perl/singleton.pl) | ✅ verificado | Lexical package state + accessor. |
| R | Applicable | [`singleton.R`](../src/DataScience/R/singleton.R) | ✅ verificado | Environment compartido dentro del proceso. |
| PowerShell | Applicable | [`singleton.ps1`](../src/Shell/PowerShell/singleton.ps1) | ✅ verificado | Script-scoped state + accessor bajo StrictMode. |
| HTML | N/A | — | — | Markup declarativo; cualquier Singleton ejecutable pertenece al runtime. |
| Assembly | Applicable | [`singleton.asm`](../src/LowLevel/Assembly/singleton.asm) | ✅ verificado | Símbolo/buffer único en la imagen del proceso + accessor. |
| Elixir | Applicable | [`singleton.exs`](../src/Functional/Elixir/singleton.exs) | ✅ verificado | `Agent` registrado por nombre dentro de la VM. |
| Shell | Applicable | [`singleton.sh`](../src/Shell/Bash/singleton.sh) | ✅ verificado | Binding único dentro del proceso shell con accessor explícito. |
| Erlang | Applicable | [`singleton.erl`](../src/Functional/Erlang/singleton.erl) | ✅ verificado | Proceso registrado único dentro de la VM. |
| Clojure | Applicable | [`singleton.clj`](../src/Functional/Clojure/singleton.clj) | ✅ verificado | `defonce` + `atom` compartida en el runtime. |
| Common Lisp | Applicable | [`singleton.lisp`](../src/Functional/Lisp/singleton.lisp) | ✅ verificado | Cell global encapsulada por accessor. |
| Prolog | Applicable | [`singleton.pl`](../src/Niche/Prolog/singleton.pl) | ✅ verificado | Predicado dinámico único + accessor lógico. |
| Delphi | Applicable | [`SingletonExample.pas`](../src/Enterprise/Delphi/SingletonExample.pas) | ✅ verificado | Class var + class function + constructor controlado; source-contract proporcional cuando DCC no está disponible. |
| GNU Octave | Applicable | [`singleton.m`](../src/DataScience/Octave/singleton.m) | ✅ verificado | `persistent` encapsulado por accessor/operaciones. |
| SQL | N/A | — | — | Unicidad de filas/datos no equivale a una instancia runtime Singleton. |
| CSS | N/A | — | — | Reglas declarativas sin ciclo de vida de instancia runtime. |
| MicroPython | Applicable | [`singleton.py`](../src/Other/MicroPython/singleton.py) | ✅ verificado | Cache de instancia ejecutada en Unix port oficial 1.28.0. |
| Rockstar | Applicable | [`singleton.rock`](../src/Other/Rockstar/singleton.rock) | ✅ verificado | Estado único mediante keyed array del runtime y accessor. |

### Clasificación N/A

- **HTML:** markup declarativo; cualquier Singleton ejecutable pertenece al runtime que lo consume.
- **CSS:** reglas declarativas de estilo; no define una instancia runtime compartida.
- **SQL declarativo:** puede imponer unicidad de datos, pero eso no equivale al patrón Singleton de instancia/ciclo de vida; no se usará un dialecto procedural para forzarlo.

La ausencia de clases nunca se usa como razón de N/A. Módulos, bindings, closures, actors, cells, records y otros mecanismos nativos son válidos si preservan la intención.

## Comprueba que lo entendiste

1. ¿Qué diferencia hay entre “una instancia por proceso” y “una instancia en todo el sistema distribuido”?  
2. ¿Por qué Dependency Injection puede ser preferible aunque la aplicación use una sola instancia?  
3. ¿Qué evidencia demostraría realmente que dos consumidores comparten la misma autoridad lógica?

## Resumen

- **Presión:** evitar autoridades duplicadas dentro de un alcance concreto.
- **Movimiento:** controlar creación y exponer una única instancia lógica.
- **Trade-off:** simplicidad de acceso frente a acoplamiento global y menor sustituibilidad.
- **Clave:** declarar siempre el alcance de la unicidad.

## Referencias

- Gamma, Helm, Johnson y Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software* — Singleton.
- [Patterns as Living Examples](../docs/philosophy/001-patterns-as-living-examples.md).
- [Canonical Design Pattern Authoring Standard](../docs/kb/catalog/pattern-authoring-standard.md).

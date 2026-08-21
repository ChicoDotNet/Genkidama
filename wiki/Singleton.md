# Singleton

> **Familia:** Creational  
> **Intención:** asegurar una única instancia lógica dentro de un alcance definido y ofrecer un punto de acceso controlado cuando esa unicidad resuelve una presión real.  
> **Estado:** `in-progress`  
> **Implementaciones de lenguaje:** `12/48`  
> **Cobertura de pruebas:** N/A — catálogo multilenguaje heterogéneo; se usará evidencia proporcional por target.  
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

La fuente canónica de targets mantiene **51 lenguajes**: 48 `Applicable` y 3 `N/A` provisionales. Hay **22 ejemplos materializados**; los 12 del tranche mainstream ya están promovidos con CI verde y los 10 del tranche portable esperan evidencia del head actual.

| Lenguaje | Aplicabilidad | Ejemplo | Validación actual | Nota |
|---|---|---|---|---|
| C# | Applicable | [`SingletonExample.cs`](../src/Enterprise/C%23/SingletonExample.cs) | ✅ verificado | `Lazy<T>` + constructor privado; alcance de proceso. |
| TypeScript | Applicable | [`singleton.ts`](../src/Web/TypeScriptTS/singleton.ts) | ✅ verificado | Instancia estática privada y acceso controlado. |
| Ada | Applicable | pendiente | ⏳ pendiente | Se expresará con package/elaboration o access value idiomático. |
| Solidity | Applicable | pendiente | ⏳ pendiente | Requiere justificar alcance de contrato, no unicidad global de blockchain. |
| Fortran | Applicable | pendiente | ⏳ pendiente | Módulo con estado único dentro del programa/runtime. |
| Pascal | Applicable | [`singleton.pas`](../src/Historical/Pascal/singleton.pas) | ⏳ pendiente CI | Inicialización controlada mediante class var + accessor. |
| Python | Applicable | [`singleton.py`](../src/Scripting/PythonPY/singleton.py) | ✅ verificado | `__new__` controla la única instancia de proceso. |
| Visual Basic .NET | Applicable | [`SingletonExample.vb`](../src/Enterprise/VisualBasic/SingletonExample.vb) | ⏳ pendiente CI | `Shared` + constructor privado. |
| C++ | Applicable | [`singleton.cpp`](../src/Systems/C%2B%2B/singleton.cpp) | ✅ verificado | Function-local static; inicialización segura desde C++11. |
| Objective-C | Applicable | pendiente | ⏳ pendiente | `dispatch_once`/static shared instance. |
| Java | Applicable | [`SingletonExample.java`](../src/Enterprise/Java/SingletonExample.java) | ✅ verificado | Initialization-on-demand holder idiom. |
| Rust | Applicable | [`singleton.rs`](../src/Systems/Rust/singleton.rs) | ✅ verificado | `OnceLock<Mutex<_>>` para estado compartido de proceso. |
| Zig | Applicable | pendiente | ⏳ pendiente | Binding global controlado dentro del ejecutable. |
| Go | Applicable | [`singleton.go`](../src/Systems/Go/singleton.go) | ✅ verificado | `sync.Once` + package state. |
| PHP | Applicable | [`singleton.php`](../src/Scripting/PHP/singleton.php) | ✅ verificado | Propiedad estática y constructor privado por proceso/request runtime. |
| Nim | Applicable | pendiente | ⏳ pendiente | Global/once-style initialization idiomática. |
| Dart | Applicable | pendiente | ⏳ pendiente | Factory constructor/static final compartido. |
| Kotlin | Applicable | [`SingletonExample.kt`](../src/Enterprise/Kotlin/SingletonExample.kt) | ✅ verificado | `object` nativo. |
| Swift | Applicable | [`singleton.swift`](../src/Systems/Swift/singleton.swift) | ✅ verificado | `static let shared`; inicialización segura del runtime. |
| F# | Applicable | [`singleton.fsx`](../src/Functional/F%23/singleton.fsx) | ✅ verificado | Static binding privado + acceso único. |
| Crystal | Applicable | pendiente | ⏳ pendiente | Class variable/constant initialization. |
| Lua | Applicable | [`singleton.lua`](../src/Scripting/Lua/singleton.lua) | ⏳ pendiente CI | Módulo/binding devuelve la misma tabla del proceso. |
| Haskell | Applicable | [`Singleton.hs`](../src/Functional/Haskell/Singleton.hs) | ⏳ pendiente CI | Binding `NOINLINE` con `IORef` canónica de proceso. |
| COBOL | Applicable | pendiente | ⏳ pendiente | Working-storage/program scope con acceso controlado. |
| Scala | Applicable | [`Singleton.scala`](../src/Functional/Scala/Singleton.scala) | ⏳ pendiente CI | `object` nativo. |
| Groovy | Applicable | pendiente | ⏳ pendiente | Static holder o `@Singleton` sólo si el ejemplo muestra intención, no sólo anotación. |
| Ruby | Applicable | [`singleton.rb`](../src/Scripting/RubyRB/singleton.rb) | ⏳ pendiente CI | Módulo estándar `Singleton`; constructor controlado. |
| C | Applicable | [`singleton.c`](../src/Systems/C/singleton.c) | ⏳ pendiente CI | File-static state + accessor; alcance de proceso. |
| OCaml | Applicable | pendiente | ⏳ pendiente | Module binding/ref compartida. |
| Julia | Applicable | pendiente | ⏳ pendiente | Module-level binding/ref compartida. |
| VBA | Applicable | pendiente | ⏳ pendiente | Módulo/clase real de VBA con source-contract proporcional. |
| GDScript | Applicable | pendiente | ⏳ pendiente | Autoload es mecanismo natural de singleton de escena/proyecto. |
| JavaScript | Applicable | [`singleton.js`](../src/Web/JavaScriptJS/singleton.js) | ✅ verificado | Constructor retorna instancia compartida. |
| MATLAB | Applicable | pendiente | ⏳ pendiente | `persistent` dentro de accessor o handle class. |
| Perl | Applicable | [`singleton.pl`](../src/Scripting/Perl/singleton.pl) | ⏳ pendiente CI | Lexical package state + accessor. |
| R | Applicable | pendiente | ⏳ pendiente | Environment/closure compartido dentro del proceso. |
| PowerShell | Applicable | [`singleton.ps1`](../src/Shell/PowerShell/singleton.ps1) | ⏳ pendiente CI | Script-scoped state + accessor bajo StrictMode. |
| HTML | N/A | — | — | Markup declarativo; cualquier Singleton ejecutable pertenece al runtime. |
| Assembly | Applicable | pendiente | ⏳ pendiente | Símbolo/buffer único en imagen de proceso + accessor. |
| Elixir | Applicable | pendiente | ⏳ pendiente | Proceso registrado/GenServer con nombre único dentro de la VM. |
| Shell | Applicable | [`singleton.sh`](../src/Shell/Bash/singleton.sh) | ⏳ pendiente CI | Binding único dentro del proceso shell con accessor explícito. |
| Erlang | Applicable | pendiente | ⏳ pendiente | Proceso registrado único dentro de la VM/supervision scope. |
| Clojure | Applicable | pendiente | ⏳ pendiente | `defonce`/atom con alcance de runtime. |
| Common Lisp | Applicable | pendiente | ⏳ pendiente | Special/global cell encapsulada por accessor. |
| Prolog | Applicable | pendiente | ⏳ pendiente | Predicado dinámico único/estado del proceso con accessor lógico. |
| Delphi | Applicable | pendiente | ⏳ pendiente | Class var/class function + constructor controlado; source-contract proporcional si DCC no está disponible. |
| GNU Octave | Applicable | pendiente | ⏳ pendiente | `persistent` dentro de función accessor. |
| SQL | N/A | — | — | Unicidad de filas/datos no equivale a una instancia runtime Singleton. |
| CSS | N/A | — | — | Reglas declarativas sin ciclo de vida de instancia runtime. |
| MicroPython | Applicable | pendiente | ⏳ pendiente | Módulo/cache de instancia ejecutado en Unix port oficial. |
| Rockstar | Applicable | pendiente | ⏳ pendiente | Estado único mediante variables/arrays del runtime sin forzar forma OO. |

### Clasificación N/A provisional

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
# Factory Method

> **Familia:** Creational  
> **Intención:** definir una operación estable que necesita crear un producto, dejando que una variante sustituible decida qué producto concreto construir.  
> **Estado:** `in-progress`  
> **Implementaciones de lenguaje:** `4/48`  
> **Cobertura de pruebas:** N/A — la completitud se valida por lenguaje con compile/run o evidencia proporcional; no existe una métrica homogénea transversal.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Factory Method mantiene estable **qué hace el Creator con un producto** y separa **cómo obtiene la variante concreta** que necesita para hacerlo.

## El problema

Una operación de alto nivel necesita usar un producto, pero acoplarla a un constructor concreto obliga a modificar esa operación cada vez que aparece una nueva variante. El problema no es simplemente “crear objetos”: es conservar un flujo estable mientras la decisión de creación permanece extensible.

## Fuerzas que compiten

- La lógica de alto nivel debe permanecer independiente del producto concreto.
- La selección del producto debe poder variar sin duplicar el flujo que lo consume.
- La extensión no debe exigir una gran fábrica de condicionales dentro del Creator.
- Para una única variante estable, una función o constructor directo suele ser más simple.

## La solución

Separar la creación detrás de un **factory method** o hook sustituible. El Creator ejecuta una operación estable que solicita un Product mediante ese hook y luego trabaja sólo contra su contrato. En OO el hook suele sobrescribirse; en lenguajes funcionales, dinámicos o de bajo nivel puede ser una función, closure, record de operaciones, callback, módulo o puntero a función.

La esencia es que **la operación consumidora pertenece al mismo límite conceptual que delega la creación**. Una función aislada `createX()` sin un flujo estable que la use es simplemente una fábrica, no evidencia suficiente de Factory Method.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Product` | Define el contrato que consume la operación estable. |
| `ConcreteProduct` | Implementa una variante concreta. |
| `Creator` | Contiene la operación estable que trabaja con `Product`. |
| Factory method / hook | Decide qué producto concreto recibe el Creator. |
| `ConcreteCreator` | Sustituye el hook cuando existe una representación OO explícita. |

## Cómo funciona

1. El cliente selecciona o configura una variante de Creator.
2. La operación estable del Creator solicita un Product mediante el factory method/hook.
3. La variante concreta crea el producto apropiado.
4. La operación continúa usando sólo el contrato de Product.
5. Agregar otra variante no requiere duplicar la operación estable.

## Diagrama

```mermaid
classDiagram
    Creator <|-- PostgresCreator
    Creator <|-- MySqlCreator
    Product <|.. PostgresDatabase
    Product <|.. MySqlDatabase

    class Creator {
      +useDatabase()
      #createDatabase() Product
    }
    class PostgresCreator {
      #createDatabase() Product
    }
    class MySqlCreator {
      #createDatabase() Product
    }
    class Product {
      <<interface>>
      +connect()
      +query()
    }
```

La flecha importante no es la herencia: `useDatabase()` permanece estable mientras el hook de creación cambia.

## Ejemplo mínimo

```csharp
public abstract class DatabaseCreator
{
    protected abstract IDatabase CreateDatabase();

    public void UseDatabase()
    {
        var database = CreateDatabase();
        database.Connect();
        database.Query();
    }
}
```

La implementación completa está en [`src/Enterprise/C#/FactoryMethodExample.cs`](../src/Enterprise/C%23/FactoryMethodExample.cs). Las variantes concretas sólo deciden qué `IDatabase` devuelve `CreateDatabase()`.

## Aplicación real

### Proveedores de base de datos

Un flujo de inicialización necesita conectar y consultar mediante el mismo contrato, pero el proveedor concreto cambia por configuración. Factory Method encaja cuando esa operación estable pertenece a un Creator extensible y cada variante sólo sustituye la creación.

Si el sistema únicamente necesita seleccionar un objeto en un punto de composición y no existe una operación estable en el Creator, una función factory o inyección directa puede ser suficiente.

## En Genkidama

La filosofía del repositorio identifica **database provider factory y module creation** como lugares donde Factory Method puede aparecer naturalmente. Esta página no reclama todavía un uso deliberado productivo: la auditoría no ha verificado una implementación productiva que conserve exactamente esta intención. Los ejemplos educativos por sí solos no prueban uso arquitectónico.

## Cuándo usarlo

- Una operación estable necesita un producto cuya variante concreta puede cambiar.
- La lógica consumidora no debe conocer constructores concretos.
- Nuevas variantes deben agregarse sustituyendo un hook de creación, no copiando el flujo.
- El lenguaje ofrece alguna forma idiomática de pasar o sustituir comportamiento de creación.

## Cuándo no usarlo

- Sólo existe una variante estable y un constructor directo es suficiente.
- Sólo necesitas escoger una familia completa de productos relacionados: considera Abstract Factory.
- Sólo necesitas ensamblar un producto por pasos: considera Builder.
- Una simple función factory inyectada en composición expresa todo el problema sin un Creator estable.

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Desacopla la operación estable de constructores concretos. | Añade un punto de extensión que puede ser innecesario. |
| Permite extender variantes sin duplicar el flujo consumidor. | En OO puede multiplicar tipos `ConcreteCreator`. |
| Facilita pruebas sustituyendo el hook de creación. | Una jerarquía ceremonial oculta más de lo que aclara. |
| Se traduce bien a callbacks/closures en otros paradigmas. | Llamar “Factory Method” a cualquier función `create` diluye la intención. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [Abstract Factory](AbstractFactory.md) | often implemented with | Una operación de Abstract Factory puede delegar la creación individual a factory methods; Abstract Factory conserva la coherencia de una familia. |
| [Builder](Builder.md) | alternative to | Builder varía un proceso de ensamblado paso a paso; Factory Method varía el producto creado dentro de una operación estable. |
| [Template Method](TemplateMethod.md) | collaborates with | Un template method puede incluir un factory method como uno de sus hooks variables. |
| [Prototype](Prototype.md) | alternative to | Clonar un prototipo puede sustituir el hook constructor cuando la variación se expresa mejor mediante datos preconfigurados. |

## Errores comunes y confusiones

### Confundir una simple función factory con Factory Method

`createDatabase(type)` puede ser una solución válida, pero no demuestra este patrón si no existe una operación estable que delega su necesidad de creación a un hook sustituible.

### Confundirlo con Abstract Factory

Una interfaz con `createDatabase()` y varias implementaciones de fábrica sigue creando un único tipo de producto. Abstract Factory requiere una familia de productos relacionados; Factory Method requiere preservar la operación estable alrededor de la creación variable.

### Forzar herencia donde no hace falta

Callbacks, closures, records, predicates o function pointers son representaciones legítimas cuando mantienen el flujo estable y sustituyen sólo el paso de creación.

## Cómo comprobar una implementación

- Cambiar la variante de Creator/hook cambia el producto concreto sin editar la operación estable.
- La operación consumidora sólo conoce el contrato de Product.
- Agregar una variante no obliga a duplicar la lógica estable.
- La evidencia ejecuta al menos dos variantes y observa comportamiento distinto del producto.
- La validación no se limita a buscar nombres como `Factory` o `Create`.

## Preguntas de comprensión

1. ¿Qué diferencia una factory function de Factory Method?
2. ¿Qué parte debe permanecer estable al agregar una nueva variante?
3. ¿Por qué la herencia no es requisito del patrón?
4. ¿Cuándo Abstract Factory resuelve una presión distinta?
5. ¿Qué comportamiento observable demostraría que el hook de creación realmente varía?

## Validación automatizada

Los ejemplos se agrupan en gates para que un rojo identifique el ecosistema que necesita Debt First. El antiguo gate `Pattern Factory Method Portable` no estaba siendo registrado por GitHub Actions de forma observable, por lo que sus diez targets se consolidaron en el workflow principal sin reducir ninguna comprobación:

- `Pattern Factory Method`: C#, Java, Python, JavaScript, TypeScript, Go, Rust, PHP, C, C++, Visual Basic .NET, F#, Ruby, Lua, Shell/Bash, PowerShell, Kotlin y Swift.
- `Pattern Factory Method Portable 2`: Ada, Solidity, Fortran, Pascal, Zig, Nim, Dart, Crystal, Haskell, Scala, Groovy y Objective-C.
- `Pattern Factory Method Functional`: Perl, Erlang, GNU Octave, R, Julia, OCaml, Common Lisp, Clojure, Elixir y Prolog.
- `Pattern Factory Method Final`: COBOL, Assembly, GDScript, MATLAB, MicroPython, Rockstar y contratos proporcionales de VBA/Delphi.

C#, Java, Python y JavaScript ya pasaron compile/run en un head anterior con exactamente los mismos blobs. TypeScript descubrió deuda del gate —la sintaxis de `npx` dejó de ser válida con npm 11— y el workflow fue corregido sin relajar la compilación strict. C, C++, Go, Ruby, PHP, Kotlin, Swift y Fortran tienen además prevalidación ejecutable sobre sus blobs actuales; esa evidencia reduce incertidumbre, pero la matriz espera los gates del head común antes de promover filas adicionales.

## Implementaciones por lenguaje

La fuente de targets es [`learn/_meta/catalog.yml`](../learn/_meta/catalog.yml): **45 targets v1 + 6 adicionales = 51**. Factory Method clasifica **48 Applicable** y **3 N/A**. Los 48 ejemplos ya están materializados; una fila cuenta como implementada sólo después de evidencia suficiente.

| Lenguaje | Aplicabilidad | Ejemplo | Validación | Estado |
|---|---|---|---|---|
| C# | Applicable | [`FactoryMethodExample.cs`](../src/Enterprise/C%23/FactoryMethodExample.cs) | .NET 10 compile/run | ✅ verificado |
| TypeScript | Applicable | [`factory_method.ts`](../src/Web/TypeScriptTS/factory_method.ts) | TS strict + Node | ⏳ rerun |
| Ada | Applicable | [`factory_method.adb`](../src/Historical/Ada/factory_method.adb) | GNAT Ada 2022 compile/run | ⏳ rerun |
| Solidity | Applicable | [`FactoryMethod.sol`](../src/Niche/Solidity/FactoryMethod.sol) | solc 0.8.30 artifact | ⏳ rerun |
| Fortran | Applicable | [`factory_method.f90`](../src/Historical/Fortran/factory_method.f90) | Fortran 2018 warnings-as-errors | ⏳ rerun |
| Pascal | Applicable | [`factory_method.pas`](../src/Historical/Pascal/factory_method.pas) | Free Pascal compile/run | ⏳ rerun |
| Python | Applicable | [`factory_method.py`](../src/Scripting/PythonPY/factory_method.py) | Python 3.14 compile/run | ✅ verificado |
| Visual Basic .NET | Applicable | [`FactoryMethodExample.vb`](../src/Enterprise/VisualBasic/FactoryMethodExample.vb) | .NET 10 compile/run | ⏳ rerun |
| C++ | Applicable | [`factory_method.cpp`](../src/Systems/C%2B%2B/factory_method.cpp) | C++20 warnings-as-errors | ⏳ rerun |
| Objective-C | Applicable | [`factory_method.m`](../src/Systems/Objective-C/factory_method.m) | macOS Clang/ARC/Foundation | ⏳ rerun |
| Java | Applicable | [`FactoryMethodExample.java`](../src/Enterprise/Java/FactoryMethodExample.java) | Java 25 `-Werror` compile/run | ✅ verificado |
| Rust | Applicable | [`factory_method.rs`](../src/Systems/Rust/factory_method.rs) | rustfmt + rustc warnings-as-errors | ⏳ rerun |
| Zig | Applicable | [`factory_method.zig`](../src/Systems/Zig/factory_method.zig) | Zig fmt/run | ⏳ rerun |
| Go | Applicable | [`factory_method.go`](../src/Systems/Go/factory_method.go) | gofmt/vet/run | ⏳ rerun |
| PHP | Applicable | [`factory_method.php`](../src/Scripting/PHP/factory_method.php) | PHP lint/run | ⏳ rerun |
| Nim | Applicable | [`factory_method.nim`](../src/Niche/Nim/factory_method.nim) | Nim compile/run | ⏳ rerun |
| Dart | Applicable | [`factory_method.dart`](../src/Web/Dart/factory_method.dart) | format/analyze/run | ⏳ rerun |
| Kotlin | Applicable | [`FactoryMethodExample.kt`](../src/Enterprise/Kotlin/FactoryMethodExample.kt) | kotlinc/JVM run | ⏳ rerun |
| Swift | Applicable | [`factory_method.swift`](../src/Systems/Swift/factory_method.swift) | swiftc compile/run | ⏳ rerun |
| F# | Applicable | [`factory_method.fsx`](../src/Functional/F%23/factory_method.fsx) | dotnet fsi | ⏳ rerun |
| Crystal | Applicable | [`factory_method.cr`](../src/Niche/Crystal/factory_method.cr) | format/build/run | ⏳ rerun |
| Lua | Applicable | [`factory_method.lua`](../src/Scripting/Lua/factory_method.lua) | Lua 5.4 run | ⏳ rerun |
| Haskell | Applicable | [`FactoryMethod.hs`](../src/Functional/Haskell/FactoryMethod.hs) | runghc `-Wall -Werror` | ⏳ rerun |
| COBOL | Applicable | [`factory_method.cbl`](../src/Historical/Cobol/factory_method.cbl) | GnuCOBOL compile/run | ⏳ rerun |
| Scala | Applicable | [`FactoryMethod.scala`](../src/Functional/Scala/FactoryMethod.scala) | scalac/run | ⏳ rerun |
| Groovy | Applicable | [`factory_method.groovy`](../src/Scripting/Groovy/factory_method.groovy) | groovyc/run | ⏳ rerun |
| Ruby | Applicable | [`factory_method.rb`](../src/Scripting/RubyRB/factory_method.rb) | syntax/run | ⏳ rerun |
| C | Applicable | [`factory_method.c`](../src/Systems/C/factory_method.c) | C17 warnings-as-errors | ⏳ rerun |
| OCaml | Applicable | [`factory_method.ml`](../src/Functional/OCaml/factory_method.ml) | ocamlc warnings-as-errors | ⏳ rerun |
| Julia | Applicable | [`factory_method.jl`](../src/DataScience/Julia/factory_method.jl) | Julia run | ⏳ rerun |
| VBA | Applicable | [`factory_method.bas`](../src/Shell/VBA/factory_method.bas) + class modules | source contract sobre VBA real | ⏳ rerun |
| GDScript | Applicable | [`factory_method.gd`](../src/Niche/GDScript/factory_method.gd) | Godot 4.6.3 headless | ⏳ rerun |
| JavaScript | Applicable | [`factory_method.js`](../src/Web/JavaScriptJS/factory_method.js) | Node 24 syntax/run | ✅ verificado |
| MATLAB | Applicable | [`factory_method.m`](../src/DataScience/MATLAB/factory_method.m) | MathWorks Actions | ⏳ rerun |
| Perl | Applicable | [`factory_method.pl`](../src/Scripting/Perl/factory_method.pl) | Perl syntax/run | ⏳ rerun |
| R | Applicable | [`factory_method.R`](../src/DataScience/R/factory_method.R) | Rscript | ⏳ rerun |
| PowerShell | Applicable | [`factory_method.ps1`](../src/Shell/PowerShell/factory_method.ps1) | pwsh strict/run | ⏳ rerun |
| Assembly | Applicable | [`factory_method.asm`](../src/LowLevel/Assembly/factory_method.asm) | NASM + ld + run | ⏳ rerun |
| Elixir | Applicable | [`factory_method.exs`](../src/Functional/Elixir/factory_method.exs) | elixirc warnings-as-errors + run | ⏳ rerun |
| Shell | Applicable | [`factory_method.sh`](../src/Shell/Bash/factory_method.sh) | bash syntax/run | ⏳ rerun |
| Erlang | Applicable | [`factory_method.erl`](../src/Functional/Erlang/factory_method.erl) | erlc `-Werror` + run | ⏳ rerun |
| Clojure | Applicable | [`factory_method.clj`](../src/Functional/Clojure/factory_method.clj) | Clojure run | ⏳ rerun |
| Common Lisp | Applicable | [`factory_method.lisp`](../src/Functional/Lisp/factory_method.lisp) | SBCL load/run | ⏳ rerun |
| Prolog | Applicable | [`factory_method.pl`](../src/Niche/Prolog/factory_method.pl) | SWI-Prolog run | ⏳ rerun |
| Delphi | Applicable | [`FactoryMethod.pas`](../src/Enterprise/Delphi/FactoryMethod.pas) | source contract sobre Delphi real | ⏳ rerun |
| GNU Octave | Applicable | [`factory_method.m`](../src/DataScience/Octave/factory_method.m) | Octave run | ⏳ rerun |
| MicroPython | Applicable | [`factory_method.py`](../src/Other/MicroPython/factory_method.py) | MicroPython 1.28.0 Unix port | ⏳ rerun |
| Rockstar | Applicable | [`factory_method.rock`](../src/Other/Rockstar/factory_method.rock) | Rockstar 2.0.31 pinned runtime | ⏳ rerun |
| HTML | N/A | — | Markup declarativo; el hook ejecutable pertenece a otro runtime/lenguaje. | N/A |
| SQL | N/A | — | SQL declarativo; no se fuerza un dialecto procedural bajo la etiqueta SQL. | N/A |
| CSS | N/A | — | Presentación declarativa; no expresa por sí sola una operación runtime con creación sustituible. | N/A |

La página permanece `in-progress` hasta que **implemented == applicable == 48**.
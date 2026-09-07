# Strategy

> **Familia:** Behavioral  
> **Intención:** Encapsular algoritmos o políticas intercambiables detrás de un mismo contrato para poder elegirlos sin cambiar al consumidor.  
> **Estado:** `in-progress`  
> **Implementaciones de lenguaje:** `36/49` Applicable con canónico individual direccionable verificado; 13 Applicable aún requieren extracción, implementación, delegación o reparación canónica.  
> **Cobertura de pruebas:** `N/A` agregada — la matriz polyglot usa compile/analyze/runtime/source-contract según ecosistema; el piso de 44% aplica donde exista coverage significativo.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Strategy separa **qué algoritmo o política usar** de **quién necesita el resultado**, de modo que el consumidor pueda cambiar de comportamiento por composición en lugar de crecer como una cadena de condicionales.

## El problema

Un consumidor necesita realizar la misma operación conceptual —calcular un precio, elegir una ruta, autenticar, ordenar, comprimir o notificar— pero existen varias políticas válidas. Cuando la selección se codifica con `if/switch` dentro del consumidor, cada algoritmo nuevo obliga a editarlo, mezcla selección con ejecución y vuelve más difícil probar cada variante de forma aislada.

La presión de diseño no es simplemente «tener dos funciones». Es que **varias políticas representan la misma responsabilidad, deben ser intercambiables y el contexto no debería conocer sus detalles internos**.

## Fuerzas que compiten

- Las variantes deben compartir un contrato suficientemente estable para ser sustituibles.
- El consumidor debe permanecer pequeño y ajeno a detalles de cada algoritmo.
- Agregar una estrategia no debería exigir reescribir el contexto.
- La selección puede ocurrir en configuración, composición, runtime o por datos de entrada.
- En dominios simples una función pasada como parámetro puede ser mejor que una jerarquía de clases.
- La abstracción no debe ocultar diferencias semánticas que en realidad requieren contratos distintos.

## La solución

Definir un contrato de estrategia y hacer que cada algoritmo lo implemente —mediante interfaces, funciones de orden superior, closures, traits, módulos, punteros a función, mensajes, predicados u otro mecanismo idiomático—. El contexto recibe la estrategia y delega en ella la parte variable.

Strategy no exige clases. En muchos targets de Genkidama, una función pasada como valor es la representación más directa y pedagógica del patrón.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Context` | Usa una política sin conocer su algoritmo interno. |
| `Strategy` | Define el contrato común de la variación. |
| `ConcreteStrategy` | Implementa una política concreta respetando el contrato. |
| Composición / cliente | Elige qué estrategia entregar al contexto. |

## Cómo funciona

1. El contexto recibe una estrategia compatible.
2. Cuando necesita la operación variable, delega en ella.
3. Una estrategia concreta ejecuta su algoritmo y devuelve el resultado según el contrato.
4. El cliente puede sustituirla por otra estrategia sin cambiar la lógica central del contexto.
5. Las estrategias se prueban por comportamiento observable y sustituibilidad, no por su forma sintáctica.

## Diagrama

```mermaid
flowchart LR
    Client[Cliente / composición] -->|elige| Context
    Context -->|delega| Strategy{Strategy}
    Strategy --> Regular[Regular]
    Strategy --> Vip[VIP]
    Strategy --> Campaign[Campaña]
```

La relación importante es que el contexto **elige o recibe una política intercambiable**; no cambia su ciclo de vida interno como ocurre en State.

## Ejemplo mínimo

```csharp
public static decimal Price(decimal amount, Func<decimal, decimal> strategy) =>
    strategy(amount);

var regular = Price(100m, value => value);
var vip = Price(100m, value => value * 0.8m);
```

El contrato es pequeño: el contexto sabe invocar una política de precio, no cómo está implementada.

## Aplicación real

### Motor de precios

Un checkout puede calcular precio regular, precio VIP o una campaña especial. Todas las variantes reciben el mismo monto base y producen un precio. Strategy permite probar y sustituir esas políticas sin convertir el checkout en el dueño de sus reglas internas.

Si sólo existe una fórmula estable o una única bifurcación trivial que no crecerá, una función local o un `if` puede ser más claro.

## En Genkidama

La filosofía del repositorio menciona Strategy como una opción natural para canales de notificación y modos de autenticación, pero esta reconciliación todavía no acredita un uso productivo deliberado específico sin enlazar primero una implementación real. Por ahora, la ficha no fuerza arquitectura productiva ni convierte similitudes accidentales en evidencia del patrón.

## Cuándo usarlo

- Existen varias políticas intercambiables para una misma responsabilidad.
- El consumidor crece con condicionales que seleccionan algoritmos.
- Se necesita sustituir comportamiento por configuración, composición o runtime.
- Cada variante merece pruebas aisladas con el mismo contrato de entrada/salida.

## Cuándo no usarlo

- Sólo hay una implementación y no existe una presión real de variación.
- Una expresión o función local comunica mejor una bifurcación pequeña.
- El comportamiento cambia debido al ciclo de vida interno del objeto; eso suele ser State.
- Las variantes no son realmente sustituibles porque requieren contratos o invariantes distintos.

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Reduce condicionales de selección dentro del contexto. | Introduce una frontera adicional de composición. |
| Hace sustituibles y comprobables las políticas. | Una estrategia por caso trivial puede convertirse en ceremonial. |
| Permite agregar variantes con impacto localizado. | Un contrato demasiado genérico puede esconder diferencias importantes. |
| Funciona bien con interfaces, funciones, closures y otros mecanismos nativos. | La selección de estrategia sigue necesitando un owner claro. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [State](State.md) | often confused with | Ambos delegan comportamiento; Strategy cambia una política elegida, State cambia comportamiento por el estado/ciclo de vida actual. |
| [Dependency Injection](DependencyInjection.md) | collaborates with | DI suele ser el mecanismo que compone o selecciona una estrategia concreta. |
| [Factory Method](FactoryMethod.md) | collaborates with | Una factory puede resolver qué estrategia construir cuando la selección requiere lógica de creación. |
| [Template Method](TemplateMethod.md) | alternative to | Template Method varía pasos por herencia/override; Strategy varía comportamiento por composición. |
| [Null Object](NullObject.md) | collaborates with | Un Null Object puede ser una estrategia válida que evita checks especiales cuando el comportamiento neutro es legítimo. |

## Errores comunes y confusiones

### Confundir Strategy con State

La estructura puede parecer idéntica. La diferencia está en la causa de la variación: Strategy responde «¿qué política quiero usar?»; State responde «¿qué comportamiento corresponde ahora dado mi estado actual?».

### Crear una clase por cada lambda trivial

Si el lenguaje soporta funciones de primera clase, closures o módulos, una jerarquía OO puede añadir ruido sin mejorar el contrato.

### Estrategias que no son sustituibles

Si una variante necesita entradas distintas, muta invariantes diferentes o devuelve conceptos incompatibles, probablemente no comparte realmente el mismo Strategy contract.

### Ocultar la selección

Extraer algoritmos pero mantener un `switch` distribuido por todo el sistema sólo mueve el problema. La composición/selección debe tener un owner claro.

## Cómo comprobar una implementación

- Al menos dos estrategias distintas satisfacen el mismo contrato.
- El contexto obtiene resultados diferentes al sustituir únicamente la estrategia.
- El contexto no contiene detalles internos de las estrategias concretas.
- Cada estrategia protege su comportamiento relevante, incluyendo failure modes cuando existan.
- Agregar una nueva estrategia no exige modificar el algoritmo interno de las anteriores.

## Validación automatizada

El head `a5b9012be8096513d27fb34e53a0f3f3812a4bbb` completó Quality, Product CI y Polyglot CI en verde. Python, Haskell y Go permanecen delegados a sus canónicos. Dart ejecuta `verifyStrategy()` desde `pattern_sweep.dart`, conserva el contrato agregado `Dart pattern sweep: 39/39 examples passed` y queda acreditado sin una segunda implementación inline.

Crystal, Zig, Julia y Objective-C ya tienen canónicos individuales materializados y enlazados abajo. Se mantienen pendientes de crédito final mientras sus sweeps conserven la implementación Strategy histórica en vez de delegar al canónico. Ningún `pattern_sweep.*` se acredita como sustituto de un canónico direccionable.

## Implementaciones por lenguaje

La fuente de targets es [`learn/_meta/catalog.yml`](../learn/_meta/catalog.yml): 45 lenguajes v1 y 6 adicionales. Clasificación: **49 Applicable + 2 N/A**.

| Lenguaje | Aplicabilidad | Ejemplo verificado / pendiente | Validación | Nota |
|---|---|---|---|---|
| C# | Applicable | [`Strategy.cs`](../src/Enterprise/C%23/patterns/Strategy.cs) | materializado; VERIFY horizontal pendiente | Delegado/función intercambiable. |
| TypeScript | Applicable | [`strategy.ts`](../src/Web/TypeScriptTS/patterns/strategy.ts) | materializado; VERIFY horizontal pendiente | Función de orden superior. |
| Ada | Applicable | [`strategy_pattern.adb`](../src/Systems/Ada/strategy_pattern.adb) | materializado; VERIFY horizontal pendiente | Access-to-function. |
| Solidity | Applicable | [`Strategy.sol`](../src/Niche/Solidity/patterns/Strategy.sol) | materializado; VERIFY horizontal pendiente | Política seleccionable. |
| Fortran | Applicable | [`strategy.f90`](../src/Systems/Fortran/patterns/strategy.f90) | materializado; VERIFY horizontal pendiente | Procedimiento intercambiable. |
| Pascal | Applicable | [`strategy_pattern.pas`](../src/Systems/Pascal/strategy_pattern.pas) | materializado; VERIFY horizontal pendiente | Procedural type/callback. |
| Python | Applicable | [`strategy.py`](../src/Scripting/PythonPY/strategy.py) | `py_compile` + runtime; Polyglot verde | Función de orden superior; sweep delegado al canónico. |
| Visual Basic .NET | Applicable | [`Strategy.vb`](../src/Enterprise/VB.NET/patterns/Strategy.vb) | materializado; VERIFY horizontal pendiente | Delegate/interfaz. |
| C++ | Applicable | [`strategy.cpp`](../src/Systems/C%2B%2B/patterns/strategy.cpp) | compile + runtime; Polyglot verde | `std::function` intercambiable. |
| Objective-C | Applicable | [`strategy.m`](../src/Systems/Objective-C/patterns/strategy.m) | materializado; delegación del sweep pendiente | Puede expresarse idiomáticamente con block. |
| Java | Applicable | [`strategy.java`](../src/Enterprise/Java/patterns/strategy.java) | `javac -Xlint:all -Werror` + runtime; Polyglot verde | `IntUnaryOperator` pasado al mismo contexto. |
| Rust | Applicable | [`strategy.rs`](../src/Systems/Rust/patterns/strategy.rs) | compile + runtime; Polyglot verde | Closure genérica `Fn`. |
| Zig | Applicable | [`strategy.zig`](../src/Systems/Zig/patterns/strategy.zig) | materializado; delegación del sweep pendiente | Function pointer/comptime strategy. |
| Go | Applicable | [`strategy.go`](../src/Systems/Go/strategy.go) | `gofmt` + `go vet` + runtime; Polyglot verde | Function value pasado al contexto; sweep delegado al canónico. |
| PHP | Applicable | [`strategy.php`](../src/Scripting/PHP/patterns/strategy.php) | materializado; VERIFY horizontal pendiente | Callable/closure. |
| Nim | Applicable | [`strategy_example.nim`](../src/Niche/Nim/patterns/strategy_example.nim) | materializado; VERIFY horizontal pendiente | Proc value. |
| Dart | Applicable | [`strategy.dart`](../src/Web/Dart/patterns/strategy.dart) | analyzer + runtime/sweep 39/39; Polyglot verde en `a5b9012...` | Function value; sweep delegado a `verifyStrategy()`. |
| Kotlin | Applicable | [`Strategy.kt`](../src/Enterprise/Kotlin/patterns/Strategy.kt) | materializado; VERIFY horizontal pendiente | Lambda/interface. |
| Swift | Applicable | [`Strategy.swift`](../src/Systems/Swift/patterns/Strategy.swift) | materializado; VERIFY horizontal pendiente | Closure/protocol. |
| F# | Applicable | [`Strategy.fsx`](../src/Functional/F%23/patterns/Strategy.fsx) | materializado; VERIFY horizontal pendiente | Función de orden superior. |
| Crystal | Applicable | [`strategy.cr`](../src/Niche/Crystal/patterns/strategy.cr) | materializado; delegación del sweep pendiente | Proc/objeto intercambiable. |
| Lua | Applicable | [`strategy.lua`](../src/Scripting/Lua/patterns/strategy.lua) | materializado; VERIFY horizontal pendiente | Funciones en tabla. |
| Haskell | Applicable | [`Strategy.hs`](../src/Functional/Haskell/patterns/Strategy.hs) | canónico ejecutado por sweep; Long-tail y Polyglot verdes | Función como estrategia; runner deduplicado. |
| COBOL | Applicable | [`strategy_pattern.cpy`](../src/Historical/Cobol/patterns/strategy_pattern.cpy) | materializado; VERIFY horizontal pendiente | Dispatch procedural. |
| Scala | Applicable | [`Strategy.scala`](../src/Functional/Scala/patterns/Strategy.scala) | materializado; VERIFY horizontal pendiente | Function value/trait. |
| Groovy | Applicable | [`strategy.groovy`](../src/Functional/Groovy/patterns/strategy.groovy) | runtime individual en JVM cohort; Polyglot verde | Closure pasada al contexto `choose`. |
| Ruby | Applicable | [`strategy.rb`](../src/Scripting/Ruby/patterns/strategy.rb) | materializado; VERIFY horizontal pendiente | Proc/module function. |
| C | Applicable | [`strategy.c`](../src/Systems/C/patterns/strategy.c) | compile + runtime; Polyglot verde | Function pointer pasado al contexto. |
| OCaml | Applicable | [`strategy.ml`](../src/Functional/OCaml/patterns/strategy.ml) | materializado; VERIFY horizontal pendiente | Función de orden superior. |
| Julia | Applicable | [`strategy.jl`](../src/DataScience/Julia/patterns/strategy.jl) | materializado; delegación del sweep pendiente | Function value/multiple dispatch. |
| VBA | Applicable | — | pendiente | Function dispatch/módulo; host Office puede limitar runtime CI. |
| GDScript | Applicable | — | pendiente | Callable/objeto strategy. |
| JavaScript | Applicable | [`strategy.js`](../src/Web/JavaScriptJS/patterns/strategy.js) | materializado; VERIFY horizontal pendiente | Función de primera clase. |
| MATLAB | Applicable | [`strategy.m`](../src/DataScience/MATLAB/strategy.m) | materializado; VERIFY horizontal pendiente | Function handle. |
| Perl | Applicable | — | pendiente | Coderef/subrutina. |
| R | Applicable | [`strategy.R`](../src/DataScience/R/patterns/strategy.R) | materializado; VERIFY horizontal pendiente | Función como argumento. |
| PowerShell | Applicable | [`strategy.ps1`](../src/Scripting/PowerShell/patterns/strategy.ps1) | materializado; VERIFY horizontal pendiente | ScriptBlock. |
| HTML | N/A | — | — | Markup estático no posee un mecanismo ejecutable para seleccionar/invocar algoritmos; JavaScript es target separado. |
| Assembly | Applicable | — | pendiente | Tabla/puntero de función o dispatch de rutina. |
| Elixir | Applicable | [`strategy.exs`](../src/Functional/Elixir/patterns/strategy.exs) | materializado; VERIFY horizontal pendiente | Función/MFA intercambiable. |
| Shell | Applicable | [`strategy.sh`](../src/Scripting/Bash/patterns/strategy.sh) | materializado; VERIFY horizontal pendiente | Nombre de función/comando como estrategia. |
| Erlang | Applicable | [`strategy.erl`](../src/Functional/Erlang/patterns/strategy.erl) | materializado; VERIFY horizontal pendiente | Fun/MFA. |
| Clojure | Applicable | [`strategy.clj`](../src/Functional/Clojure/patterns/strategy.clj) | materializado; VERIFY horizontal pendiente | Función como valor. |
| Common Lisp | Applicable | [`strategy.lisp`](../src/Functional/CommonLisp/patterns/strategy.lisp) | materializado; VERIFY horizontal pendiente | Function designator. |
| Prolog | Applicable | — | pendiente | Predicado/política seleccionable. |
| Delphi | Applicable | — | pendiente | Method pointer/interface/procedure variable. |
| GNU Octave | Applicable | [`strategy.m`](../src/DataScience/Octave/patterns/strategy.m) | materializado; VERIFY horizontal pendiente | Function handle. |
| SQL | Applicable | — | pendiente | Política puede expresarse como relación/tabla seleccionable o query strategy sin exigir OO. |
| CSS | N/A | — | — | CSS selecciona reglas de estilo, no invoca algoritmos intercambiables como responsabilidad ejecutable autónoma. |
| MicroPython | Applicable | — | pendiente | Callable/función intercambiable. |
| Rockstar | Applicable | — | pendiente | Funciones seleccionables si el runtime soporta el contrato necesario. |

## Comprueba que lo entendiste

1. ¿Por qué dos algoritmos detrás del mismo `switch` no son todavía una buena aplicación de Strategy si el consumidor sigue conociendo todos sus detalles?
2. Si dos implementaciones tienen la misma forma de clases pero una cambia por configuración y otra por ciclo de vida interno, ¿cuál es Strategy y cuál State?
3. ¿Cuándo una lambda pasada como parámetro comunica Strategy mejor que crear una interfaz y varias clases?

## Resumen

- Strategy separa una política intercambiable del contexto que la usa.
- La sustituibilidad del contrato importa más que la forma OO.
- Funciones, closures, traits, punteros, módulos y predicados pueden ser implementaciones idiomáticas.
- State es el vecino más fácil de confundir: cambia por estado interno, no por elección de política.
- La matriz está en progreso: 36/49 Applicable tienen canónico individual verificado; 13 requieren cierre antes de `validated`.

## Referencias

- Gamma, Helm, Johnson, Vlissides — *Design Patterns: Elements of Reusable Object-Oriented Software*.
- [`docs/philosophy/001-patterns-as-living-examples.md`](../docs/philosophy/001-patterns-as-living-examples.md)
- [`docs/kb/catalog/pattern-authoring-standard.md`](../docs/kb/catalog/pattern-authoring-standard.md)
# Command

> **Familia:** Behavioral  
> **Intención:** encapsular una solicitud o acción como un valor autónomo para desacoplar quién la solicita de quién la ejecuta y permitir tratarla como dato cuando se necesita encolar, registrar, parametrizar o deshacer.  
> **Estado:** `in-progress`  
> **Implementaciones de lenguaje:** `1/49`  
> **Cobertura de pruebas:** N/A — el catálogo usa ejemplos standalone multi-ecosistema; para MATLAB se valida comportamiento real con MATLAB Actions en lugar de inventar un porcentaje agregado.  
> **Mapa:** [Volver al catálogo y mapa de relaciones](README.md)

## En una frase

Command convierte una operación en algo que puede guardarse, pasarse, ordenarse y ejecutarse después sin que el invocador conozca cómo se realiza.

## El problema

Un sistema administrativo permite depositar y retirar saldo. La interfaz que recibe la intención del usuario no debería contener la lógica de cada operación ni necesitar una rama nueva cada vez que aparece otra acción. Además, algunas operaciones deben poder ponerse en cola, auditarse, repetirse o revertirse.

Una llamada directa como `account.withdraw(20)` es suficiente cuando no existe esa presión. El problema aparece cuando la **operación misma necesita identidad y ciclo de vida**: debe viajar por una cola, conservar parámetros, registrarse antes de ejecutarse, dispararse desde distintos invocadores o formar parte de un historial.

## Fuerzas que compiten

- El invocador no debería conocer la implementación concreta que modifica al receptor.
- La solicitud necesita conservar sus parámetros hasta el momento de ejecución.
- Distintos tipos de operación deben poder recorrer la misma cola, historial o mecanismo de despacho.
- El orden de ejecución puede ser comportamiento observable y debe permanecer explícito.
- Algunas operaciones admiten `undo`, pero otras son irreversibles o requieren compensación en vez de una inversión exacta.
- Encapsular cada llamada trivial como Command agrega objetos, datos o funciones que no se justifican si la operación nunca necesita tratarse como valor.

## La solución

Representar cada solicitud como un **Command** que contiene la información necesaria para ejecutar una intención. Un **Invoker** decide cuándo ejecutarla, pero no implementa la operación. El **Receiver** o handler conoce el trabajo real. Una cola o historial puede conservar comandos porque la solicitud ya no es sólo una llamada efímera: tiene una representación propia.

La intención no depende de clases. Un Command puede ser un objeto, record, closure, mensaje, estructura con function handles, discriminated union, tabla de datos o cualquier representación que conserve la operación y sus parámetros de forma que un invocador común pueda ejecutarla después.

## Participantes y responsabilidades

| Participante | Responsabilidad |
|---|---|
| `Command` | Representa una solicitud ejecutable y conserva sus parámetros. |
| `ConcreteCommand` | Vincula una intención concreta con la operación necesaria para cumplirla. |
| `Invoker` | Decide cuándo ejecutar, encolar, registrar o repetir un Command sin implementar su lógica de negocio. |
| `Receiver` / handler | Conoce cómo producir el cambio real solicitado por el Command. |
| Cola / historial | Opcionalmente conserva Commands para ejecución diferida, auditoría, retry o undo. |

## Cómo funciona

1. El cliente crea dos operaciones: `deposit(50)` y `withdraw(20)`.
2. El invocador las conserva en una cola sin ejecutar todavía ninguna lógica del receptor.
3. Al recorrer la cola, cada Command usa su operación asociada contra la misma cuenta.
4. El saldo pasa de `100` a `150` y después a `130`; el orden forma parte del resultado.
5. El historial conserva el último Command y su semántica de reversión.
6. Al deshacer `withdraw(20)`, se aplica su operación inversa y el saldo vuelve a `150`.

## Diagrama

```mermaid
sequenceDiagram
    participant C as Cliente
    participant I as Invoker / cola
    participant D as deposit(50)
    participant W as withdraw(20)
    participant A as Account
    C->>I: enqueue(D)
    C->>I: enqueue(W)
    I->>D: execute()
    D->>A: deposit(50)
    I->>W: execute()
    W->>A: withdraw(20)
    Note over A: balance = 130
    I->>W: undo()
    W->>A: deposit(20)
    Note over A: balance = 150
```

Lo importante no es la forma de clase, sino que el invocador manipula las solicitudes como valores y que la cuenta recibe el trabajo sólo cuando el Command se ejecuta.

## Ejemplo mínimo

```text
queue = [deposit(50), withdraw(20)]

execute(queue)
=> balance=130

undo(last(queue))
=> balance=150
```

El invocador puede conservar y recorrer ambas operaciones mediante el mismo mecanismo. No necesita un `if` que conozca cómo depositar o retirar.

## Aplicación real

### Operaciones administrativas encoladas y auditables

Un backoffice puede recibir operaciones heterogéneas —crear una entidad, recalcular un documento, enviar una notificación o ejecutar una corrección— y representarlas como Commands antes de despacharlas. Esto permite separar la captura de intención de la ejecución, registrar qué se pidió y aplicar políticas comunes de retry, autorización o telemetría.

Si la llamada se ejecuta inmediatamente, nunca se almacena ni se trata de forma uniforme y el emisor conoce naturalmente al receptor, una llamada directa suele ser más clara. Command gana valor cuando la operación necesita viajar o adquirir ciclo de vida propio.

## En Genkidama

Genkidama usa Command deliberadamente en su capa de aplicación. [`IGenkidamaCommand<TResponse>`](../src/Genkidama.Application/IGenkidamaCommand.cs) marca una solicitud que cambia estado; [`IGenkidamaCommandHandler<TCommand, TResponse>`](../src/Genkidama.Application/IGenkidamaCommandHandler.cs) separa la ejecución concreta; y [`GenkidamaCommandDispatcher`](../src/Genkidama.Application/GenkidamaCommandDispatcher.cs) envía el Command al handler a través del pipeline sin implementar la operación de negocio.

[`GenkidamaCommandDispatcherTests`](../tests/Genkidama.Application.Tests/GenkidamaCommandDispatcherTests.cs) verifica ese contrato con `CreateThing`: el dispatcher recibe el Command y el handler produce `created:demo`. Esta es arquitectura existente; el catálogo no introduce infraestructura artificial para exhibir el patrón.

## Cuándo usarlo

- Una solicitud debe encolarse, diferirse, registrarse o transportarse antes de ejecutar.
- Varios tipos de operación deben pasar por un invocador, dispatcher o historial uniforme.
- El emisor debe desacoplarse de la implementación concreta que realiza el cambio.
- Se necesita retry, macro-commands, auditoría o una semántica explícita de undo/compensación.
- Los parámetros de la operación deben sobrevivir como parte de una solicitud identificable.

## Cuándo no usarlo

- Una llamada directa expresa completamente la intención y no existe necesidad de tratarla como dato.
- Crear un Command por cada método sólo añade ceremonia sin cola, despacho, historial u otra presión real.
- La operación es irreversible y el diseño pretende vender un `undo` ficticio en lugar de una compensación explícita.
- Lo que varía es únicamente el algoritmo seleccionado para una tarea estable; [Strategy](Strategy.md) suele comunicar mejor esa fuerza.
- Sólo se necesita encadenar posibles receptores hasta que uno atienda; considera [Chain of Responsibility](ChainOfResponsibility.md).

## Consecuencias y trade-offs

| A favor | Costo / riesgo |
|---|---|
| Desacopla invocador y ejecución concreta. | Introduce una representación adicional para cada tipo de solicitud. |
| Permite colas, historial, logging y ejecución diferida. | Versionar Commands persistidos puede convertirse en un contrato de datos de larga vida. |
| Hace explícitos los parámetros y el orden de operaciones. | Un Command mal diseñado puede terminar cargando demasiada lógica de dominio. |
| Puede habilitar undo, retry y macro-commands. | Undo no siempre es posible; los efectos externos suelen requerir compensación e idempotencia. |
| Permite aplicar políticas comunes alrededor del despacho. | El flujo real puede volverse menos obvio si hay demasiadas capas de dispatcher/pipeline. |

## Patrones relacionados

[Consulta también el mapa global de relaciones](README.md#relationship-map).

| Patrón | Relación | Por qué importa |
|---|---|---|
| [Chain of Responsibility](ChainOfResponsibility.md) | collaborates with | Un Command puede recorrer una cadena de handlers sin que el emisor seleccione el receptor final. |
| [Memento](Memento.md) | often implemented with | Un Command puede usar un Memento cuando undo requiere restaurar estado y una operación inversa no es suficiente. |
| [Strategy](Strategy.md) | often confused with | Strategy encapsula un algoritmo intercambiable; Command encapsula una solicitud/acción que puede adquirir ciclo de vida propio. |
| [Composite](Composite.md) | often implemented with | Un macro-command puede componer varios Commands y tratarlos como una sola operación. |

## Errores comunes y confusiones

### Llamar `Command` a un DTO sin comportamiento de despacho

Tener un objeto llamado `CreateOrderCommand` no demuestra el patrón por sí solo. Debe existir una razón para reificar la solicitud y un mecanismo que desacople su invocación de la ejecución concreta.

### Meter todo el dominio dentro del Command

El Command conserva intención y parámetros; no obliga a duplicar las reglas del receptor. En arquitecturas con handlers, éste puede coordinar el caso de uso mientras las entidades y servicios de dominio siguen siendo responsables de sus invariantes.

### Confundir Command con Strategy

Ambos pueden usar una interfaz común, pero responden a preguntas distintas. Strategy decide **cómo** realizar una tarea; Command representa **qué solicitud debe ejecutarse** y permite conservarla o despacharla después.

### Prometer undo donde sólo existe compensación

Un retiro en memoria puede revertirse exactamente. Un correo enviado o un pago externo no puede “des-enviarse”. En esos casos el Command debe exponer idempotencia, compensación o una política de error realista en lugar de fingir reversibilidad.

## Cómo comprobar una implementación

- La solicitud puede crearse y conservarse antes de ejecutar el cambio real.
- El mismo invocador puede ejecutar al menos dos Commands concretos sin conocer la implementación de sus receptores.
- Los parámetros capturados por el Command llegan sin alterarse a la ejecución.
- Cambiar el orden de Commands en una cola produce el orden de efectos esperado y verificable.
- Si se ofrece undo, retry o compensación, su semántica es explícita y se comprueba como comportamiento; no se infiere sólo por nombres de métodos.

## Implementaciones por lenguaje

Universo actual: **51 targets**. Command clasifica provisionalmente **49 Applicable** y **2 N/A**. HTML y CSS no poseen un modelo de ejecución capaz de encapsular y despachar una acción como valor. SQL declarativo permanece Applicable: una solicitud puede reificarse como datos relacionales (tipo de operación + parámetros + orden/estado) y un mecanismo SQL de despacho puede interpretar esa representación sin depender de clases.

Actualmente hay **1 ejemplo materializado y 1 verificado**. MATLAB pasó el gate nativo de MATLAB Actions; los otros 48 targets Applicable siguen pendientes dentro del mismo PR de Command durante las siguientes pasadas del experimento.

| Lenguaje / target | Aplicabilidad | Ejemplo verificado | Validación | Nota |
|---|---|---|---|---|
| C# | Applicable | — | Pendiente | objeto/record + handler/dispatcher |
| TypeScript | Applicable | — | Pendiente | objeto o closure ejecutable |
| Python | Applicable | — | Pendiente | callable/objeto + invoker |
| C++ | Applicable | — | Pendiente | value/object + execute |
| Java | Applicable | — | Pendiente | command object + receiver |
| Rust | Applicable | — | Pendiente | enum/trait + dispatcher |
| Go | Applicable | — | Pendiente | struct + función/receiver |
| PHP | Applicable | — | Pendiente | objeto callable + invoker |
| F# | Applicable | — | Pendiente | discriminated union/función |
| JavaScript | Applicable | — | Pendiente | objeto/closure + cola |
| SQL declarativo | Applicable | — | Pendiente | solicitud reificada como fila + despacho declarativo |
| Kotlin | Applicable | — | Pendiente | sealed command + handler |
| Swift | Applicable | — | Pendiente | enum/protocol + executor |
| Visual Basic .NET | Applicable | — | Pendiente | interface + handler |
| C | Applicable | — | Pendiente | struct + function pointer |
| Ruby | Applicable | — | Pendiente | objeto/Proc + invoker |
| Lua | Applicable | — | Pendiente | tabla + función |
| Bash | Applicable | — | Pendiente | datos + función de despacho |
| PowerShell | Applicable | — | Pendiente | objeto/scriptblock + dispatcher |
| Haskell | Applicable | — | Pendiente | ADT + interpreter |
| Perl | Applicable | — | Pendiente | hash/coderef + invoker |
| Pascal | Applicable | — | Pendiente | record/object + execute |
| R | Applicable | — | Pendiente | lista/closure + executor |
| GNU Octave | Applicable | — | Pendiente | struct + function handle |
| OCaml | Applicable | — | Pendiente | variant + evaluator |
| Common Lisp | Applicable | — | Pendiente | struct/list + function |
| Scala | Applicable | — | Pendiente | case class/ADT + handler |
| Julia | Applicable | — | Pendiente | struct + callable/dispatch |
| Clojure | Applicable | — | Pendiente | map + function/dispatch |
| Elixir | Applicable | — | Pendiente | struct/tuple + dispatcher |
| Erlang | Applicable | — | Pendiente | tuple/message + executor |
| Prolog | Applicable | — | Pendiente | term + dispatch predicate |
| Groovy | Applicable | — | Pendiente | object/closure + invoker |
| Ada | Applicable | — | Pendiente | tagged/record command + procedure |
| Solidity | Applicable | — | Pendiente | encoded operation + dispatcher contract |
| Fortran | Applicable | — | Pendiente | derived type + procedure dispatch |
| Objective-C | Applicable | — | Pendiente | object/block + receiver |
| Zig | Applicable | — | Pendiente | tagged union/struct + function |
| Nim | Applicable | — | Pendiente | object/ref + proc |
| Dart | Applicable | — | Pendiente | class/closure + invoker |
| Crystal | Applicable | — | Pendiente | object/proc + dispatcher |
| COBOL | Applicable | — | Pendiente | command record + paragraph dispatch |
| VBA | Applicable | — | Pendiente | class/module data + procedure dispatcher |
| GDScript | Applicable | — | Pendiente | object/Callable + queue |
| Assembly | Applicable | — | Pendiente | opcode/data record + explicit dispatcher |
| Delphi | Applicable | — | Pendiente | interface/object + execute |
| MicroPython | Applicable | — | Pendiente | callable/object + queue |
| Rockstar | Applicable | — | Pendiente | datos de operación + función de despacho |
| MATLAB | Applicable | [`command.m`](../src/DataScience/MATLAB/command.m) | Pattern Command MATLAB #2 ✅ — native MATLAB Actions; setup 92 s, validation 6 s, total 98 s | structs + function handles + cola + undo |
| HTML | N/A | — | — | markup declarativo sin modelo de ejecución para encapsular o despachar acciones |
| CSS | N/A | — | — | reglas declarativas de estilo sin solicitudes ejecutables ni invocador/receiver |

## Comprueba que lo entendiste

1. Si una interfaz llama directamente a `account.withdraw(20)` y nunca necesita encolar, registrar ni diferir esa acción, ¿qué presión faltaría para justificar Command?
2. ¿Por qué Strategy y Command pueden verse estructuralmente similares aunque Strategy encapsule un algoritmo y Command encapsule una solicitud?
3. Si un Command dispara un pago externo irreversible, ¿por qué una operación `undo()` local puede ser una promesa incorrecta y qué alternativa de diseño esperarías?

## Resumen

- Command reifica una solicitud para que pueda manipularse independientemente de su ejecución.
- Invoker y receiver quedan desacoplados, pero se paga con una representación y un flujo adicionales.
- Colas, historial, auditoría, retry y undo/compensación son presiones típicas; no requisitos que deban fingirse en todos los casos.
- Chain of Responsibility puede enrutar Commands; Memento puede ayudar a restaurar estado; Strategy se distingue por encapsular el algoritmo y no la solicitud.
- La intención es portable a paradigmas sin clases mediante records, mensajes, closures, function handles, ADTs o datos interpretables.

## Referencias

- Gamma, Helm, Johnson y Vlissides, *Design Patterns: Elements of Reusable Object-Oriented Software*.
- [Patterns as Living Examples](../docs/philosophy/001-patterns-as-living-examples.md).
- [KB-006 — Canonical Design Pattern Authoring Standard](../docs/kb/catalog/pattern-authoring-standard.md).

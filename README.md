# Design Patterns Tutorial

Bienvenido al repositorio de **Design Patterns Tutorial**. Este repositorio tiene como objetivo proporcionar explicaciones claras y ejemplos prácticos de patrones de diseño en múltiples lenguajes de programación, promoviendo su uso entre la comunidad de desarrolladores.

## Estructura del Repositorio

El repositorio está organizado en varias carpetas y archivos para facilitar la navegación y el acceso a los recursos:

- **docs/**: Documentación, diagramas UML y referencias.
  - **images/**: Imágenes y diagramas UML.
  - **references/**: Libros y artículos de referencia.
- **src/**: Código fuente para cada patrón de diseño, organizado por patrón y lenguaje.
  - **PatternName/**: Carpeta para cada patrón de diseño.
    - **Java/**: Implementaciones en Java.
    - **Python/**: Implementaciones en Python.
    - **CSharp/**: Implementaciones en C#.
    - **exampleX/**: Ejemplos específicos del patrón.
- **wiki/**: Explicaciones detalladas de cada patrón de diseño, accesibles a través del wiki de GitHub.

## Patrones de Diseño Cubiertos

Este repositorio cubre una amplia gama de patrones de diseño, incluyendo pero no limitado a:

### Patrones Creacionales
- [Abstract Factory](/src/wiki/AbstractFactory.md)
- [Builder](/src/wiki/Builder.md)
- [Factory Method](/src/wiki/FactoryMethod.md)
- [Prototype](/src/wiki/Prototype.md)
- [Singleton](/src/wiki/Singleton.md)

### Patrones Estructurales
- [Adapter](/src/wiki/Adapter.md)
- [Bridge](/src/wiki/Bridge.md)
- [Composite](/src/wiki/Composite.md)
- [Decorator](/src/wiki/Decorator.md)
- [Facade](/src/wiki/Facade.md)
- [Flyweight](/src/wiki/Flyweight.md)
- [Proxy](/src/wiki/Proxy.md)

### Patrones de Comportamiento
- [Chain of Responsibility](/src/wiki/ChainOfResponsibility.md)
- [Command](/src/wiki/Command.md)
- [Interpreter](/src/wiki/Interpreter.md)
- [Iterator](/src/wiki/Iterator.md)
- [Mediator](/src/wiki/Mediator.md)
- [Memento](/src/wiki/Memento.md)
- [Observer](/src/wiki/Observer.md)
- [State](/src/wiki/State.md)
- [Strategy](/src/wiki/Strategy.md)
- [Template Method](/src/wiki/TemplateMethod.md)
- [Visitor](/src/wiki/Visitor.md)

### Patrones Arquitectónicos
- [MVC](/src/wiki/MVC.md)
- [MVVM](/src/wiki/MVVM.md)
- [Microkernel](/src/wiki/Microkernel.md)
- [Microservices](/src/wiki/Microservices.md)

### Patrones de Integración
- [Adapter (Enterprise Integration)](/src/wiki/AdapterEnterpriseIntegration.md)
- [Bridge (Enterprise Integration)](/src/wiki/BridgeEnterpriseIntegration.md)
- [Facade (Enterprise Integration)](/src/wiki/FacadeEnterpriseIntegration.md)
- [Broker](/src/wiki/Broker.md)
- [Message Bus](/src/wiki/MessageBus.md)
- [Service Locator](/src/wiki/ServiceLocator.md)

### Patrones de Concurrencia
- [Active Object](/src/wiki/ActiveObject.md)
- [Monitor Object](/src/wiki/MonitorObject.md)
- [Half-Sync/Half-Async](/src/wiki/HalfSyncHalfAsync.md)
- [Leader/Followers](/src/wiki/LeaderFollowers.md)

### Patrones de Distribución
- [Client-Server](/src/wiki/ClientServer.md)
- [Peer-to-Peer](/src/wiki/PeerToPeer.md)
- [Publish-Subscribe](/src/wiki/PublishSubscribe.md)
- [Proxy (Distribuido)](/src/wiki/ProxyDistribuido.md)

### Patrones de Presentación
- [Presentation-Abstraction-Control](/src/wiki/PresentationAbstractionControl.md)
- [Model-View-Presenter](/src/wiki/ModelViewPresenter.md)
- [Document-View](/src/wiki/DocumentView.md)

### Patrones de Persistencia
- [Active Record](/src/wiki/ActiveRecord.md)
- [Data Mapper](/src/wiki/DataMapper.md)
- [Unit of Work](/src/wiki/UnitOfWork.md)

### Otros Patrones
- [Dependency Injection](/src/wiki/DependencyInjection.md)
- [Lazy Initialization](/src/wiki/LazyInitialization.md)
- [Object Pool](/src/wiki/ObjectPool.md)
- [Null Object](/src/wiki/NullObject.md)
- [Repository](/src/wiki/Repository.md)

## Ejemplos de Código

Cada patrón de diseño incluye múltiples ejemplos prácticos implementados en diferentes lenguajes de programación. Esto permite a los desarrolladores ver cómo se pueden aplicar estos patrones en varios contextos y lenguajes. Los lenguajes cubiertos incluyen:

- [Lenguajes Funcionales](src/Functional)
  - [Erlang](src/Functional/Erlang)
  - [Elixir](src/Functional/Elixir)
  - [Clojure](src/Functional/Clojure)
  - [Scala](src/Functional/Scala)
  - [F#](src/Functional/FSharp)
  - [Lisp](src/Functional/Lisp)
  - [OCaml](src/Functional/OCaml)
  - [Haskell](src/Functional/Haskell)
- [Lenguajes de Scripting](src/Scripting)
  - [Perl](src/Scripting/Perl)
  - [Python (PY)](src/Scripting/PythonPY)
  - [Ruby (RB)](src/Scripting/RubyRB)
  - [Lua](src/Scripting/Lua)
  - [PHP](src/Scripting/PHP)
  - [Groovy](src/Scripting/Groovy)
- [Lenguajes usados en sistemas](src/Systems)
  - [C](src/Systems/C)
  - [C++](src/Systems/C++)
  - [Rust](src/Systems/Rust)
  - [Zig](src/Systems/Zig)
  - [Go](src/Systems/Go)
  - [Swift](src/Systems/Swift)
  - [Objective-C](src/Systems/Objective-C)
- [Lenguajes usados en Enterprise Applications](src/Enterprise)
  - [Java](src/Enterprise/Java)
  - [C#](src/Enterprise/CSharp)
  - [Kotlin](src/Enterprise/Kotlin)
  - [Delphi](src/Enterprise/Delphi)
  - [Visual Basic](src/Enterprise/VisualBasic)
- [Lenguajes de DataScience](src/DataScience)
  - [R](src/DataScience/R)
  - [Julia](src/DataScience/Julia)
  - [MATLAB](src/DataScience/MATLAB)
- [Lenguajes Web](src/Web)
  - [HTML/CSS](src/Web/HTMLCSS)
  - [JavaScript (JS)](src/Web/JavaScriptJS)
  - [TypeScript (TS)](src/Web/TypeScriptTS)
  - [Dart](src/Web/Dart)
- [Databases](src/Databases)
  - [SQL](src/Databases/SQL)
- [Shell](src/Shell)
  - [Bash/Shell](src/Shell/BashShell)
  - [PowerShell](src/Shell/PowerShell)
  - [VBA](src/Shell/VBA)
- [Lenguajes Historicos](src/Historical)
  - [Fortran](src/Historical/Fortran)
  - [Cobol](src/Historical/Cobol)
  - [Ada](src/Historical/Ada)
- [Lenguajes de Nicho](src/Niche)
  - [Solidity](src/Niche/Solidity)
  - [Prolog](src/Niche/Prolog)
  - [Nim](src/Niche/Nim)
  - [Crystal](src/Niche/Crystal)
  - [GDScript](src/Niche/GDScript)
- [Lenguajes LowLevel](src/LowLevel)
  - [Assembly](src/LowLevel/Assembly)
- [Otros lenguajes](src/Other)
  - [MicroPython](src/Other/MicroPython)
  - [Rockstar](src/Other/Rockstar)

## Contribuyendo

Estamos abiertos a contribuciones de la comunidad. Para contribuir:

1. Haz un fork del repositorio.
2. Crea una nueva rama para tu característica o corrección de errores.
3. Realiza tus cambios.
4. Envía un pull request.

Por favor, asegúrate de que tu código sigue las guías de estilo y que incluyes pruebas donde sea apropiado. Consulta `CONTRIBUTING.md` para más detalles.

## Licencia

Este proyecto está licenciado bajo la licencia MIT. Consulta el archivo `LICENSE` para más detalles.

## Contacto

Si tienes alguna pregunta o sugerencia, no dudes en abrir un issue o enviar un pull request. Estamos aquí para ayudarte a aprender y aplicar patrones de diseño en tus proyectos.

---

¡Gracias por visitar el repositorio de **Design Patterns Tutorial**! Esperamos que encuentres estos recursos útiles y que te ayuden a mejorar tus habilidades de desarrollo de software.

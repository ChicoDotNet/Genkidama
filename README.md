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
- [Abstract Factory](/wiki/AbstractFactory.md)
- [Builder](/wiki/Builder.md)
- [Factory Method](/wiki/FactoryMethod.md)
- [Prototype](/wiki/Prototype.md)
- [Singleton](/wiki/Singleton.md)

### Patrones Estructurales
- [Adapter](/wiki/Adapter.md)
- [Bridge](/wiki/Bridge.md)
- [Composite](/wiki/Composite.md)
- [Decorator](/wiki/Decorator.md)
- [Facade](/wiki/Facade.md)
- [Flyweight](/wiki/Flyweight.md)
- [Proxy](/wiki/Proxy.md)

### Patrones de Comportamiento
- [Chain of Responsibility](/wiki/ChainOfResponsibility.md)
- [Command](/wiki/Command.md)
- [Interpreter](/wiki/Interpreter.md)
- [Iterator](/wiki/Iterator.md)
- [Mediator](/wiki/Mediator.md)
- [Memento](/wiki/Memento.md)
- [Observer](/wiki/Observer.md)
- [State](/wiki/State.md)
- [Strategy](/wiki/Strategy.md)
- [Template Method](/wiki/TemplateMethod.md)
- [Visitor](/wiki/Visitor.md)

### Patrones Arquitectónicos
- [MVC](/wiki/MVC.md)
- [MVVM](/wiki/MVVM.md)
- [Microkernel](/wiki/Microkernel.md)
- [Microservices](/wiki/Microservices.md)

### Patrones de Integración
- [Adapter (Enterprise Integration)](/wiki/AdapterEnterpriseIntegration.md)
- [Bridge (Enterprise Integration)](/wiki/BridgeEnterpriseIntegration.md)
- [Facade (Enterprise Integration)](/wiki/FacadeEnterpriseIntegration.md)
- [Broker](/wiki/Broker.md)
- [Message Bus](/wiki/MessageBus.md)
- [Service Locator](/wiki/ServiceLocator.md)

### Patrones de Concurrencia
- [Active Object](/wiki/ActiveObject.md)
- [Monitor Object](/wiki/MonitorObject.md)
- [Half-Sync/Half-Async](/wiki/HalfSyncHalfAsync.md)
- [Leader/Followers](/wiki/LeaderFollowers.md)

### Patrones de Distribución
- [Client-Server](/wiki/ClientServer.md)
- [Peer-to-Peer](/wiki/PeerToPeer.md)
- [Publish-Subscribe](/wiki/PublishSubscribe.md)
- [Proxy (Distribuido)](/wiki/ProxyDistribuido.md)

### Patrones de Presentación
- [Presentation-Abstraction-Control](/wiki/PresentationAbstractionControl.md)
- [Model-View-Presenter](/wiki/ModelViewPresenter.md)
- [Document-View](/wiki/DocumentView.md)

### Patrones de Persistencia
- [Active Record](/wiki/ActiveRecord.md)
- [Data Mapper](/wiki/DataMapper.md)
- [Unit of Work](/wiki/UnitOfWork.md)

### Otros Patrones
- [Dependency Injection](/wiki/DependencyInjection.md)
- [Lazy Initialization](/wiki/LazyInitialization.md)
- [Object Pool](/wiki/ObjectPool.md)
- [Null Object](/wiki/NullObject.md)
- [Repository](/wiki/Repository.md)

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

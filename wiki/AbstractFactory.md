# Abstract Factory

## Introducción

El patrón Abstract Factory es uno de los patrones de diseño creacionales que proporciona una interfaz para crear familias de objetos relacionados o dependientes sin especificar sus clases concretas. Este patrón permite a una clase delegar la responsabilidad de instanciar objetos a subclases que implementan una interfaz común.

## Explicación del Problema que Resuelve

El patrón Abstract Factory resuelve el problema de la creación de familias de productos sin acoplarse a sus clases concretas. Esto es útil en escenarios donde el sistema necesita ser independiente de cómo se crean y representan los productos. Por ejemplo, en una aplicación que soporta múltiples temas de interfaz de usuario (UI), se puede usar el patrón Abstract Factory para crear elementos de UI (botones, ventanas, etc.) específicos de cada tema sin que la lógica de la aplicación necesite conocer los detalles de implementación de cada tema.

## Diagrama

```mermaid
classDiagram
    AbstractFactory <|-- ConcreteFactory1
    AbstractFactory <|-- ConcreteFactory2
    AbstractFactory : +createProductA() ProductA
    AbstractFactory : +createProductB() ProductB
    ConcreteFactory1 : +createProductA() ProductA1
    ConcreteFactory1 : +createProductB() ProductB1
    ConcreteFactory2 : +createProductA() ProductA2
    ConcreteFactory2 : +createProductB() ProductB2
    ProductA <|-- ProductA1
    ProductA <|-- ProductA2
    ProductB <|-- ProductB1
    ProductB <|-- ProductB2

    class AbstractFactory {
      <<interface>>
      +createProductA() ProductA
      +createProductB() ProductB
    }
    class ConcreteFactory1 {
      +createProductA() ProductA1
      +createProductB() ProductB1
    }
    class ConcreteFactory2 {
      +createProductA() ProductA2
      +createProductB() ProductB2
    }
    class ProductA {
      <<interface>>
    }
    class ProductB {
      <<interface>>
    }
    class ProductA1
    class ProductA2
    class ProductB1
    class ProductB2
```

## Casos Prácticos

### Caso 1: Aplicación de gestión de UI con temas

#### Explicación

En una aplicación de interfaz de usuario (UI), es común tener diferentes temas (por ejemplo, tema oscuro y tema claro) que cambian la apariencia de los componentes de la UI. Utilizando el patrón Abstract Factory, podemos crear familias de componentes de UI específicos de cada tema sin acoplar la lógica de la aplicación a las clases concretas de los componentes.

Por ejemplo, podemos tener una fábrica abstracta `UIFactory` con métodos para crear botones y ventanas. Las fábricas concretas `DarkThemeFactory` y `LightThemeFactory` implementan estos métodos para crear componentes con el estilo correspondiente al tema oscuro y al tema claro, respectivamente.

#### Listado de Lenguajes
- [Erlang](/src/Functional/Erlang/example1.erl)
- [Elixir](/src/Functional/Elixir/example1.exs)
- [Clojure](/src/Functional/Clojure/example1.clj)
- [Scala](/src/Functional/Scala/example1.scala)
- [F#](/src/Functional/FSharp/example1.fsx)
- [Lisp](/src/Functional/Lisp/example1.lisp)
- [OCaml](/src/Functional/OCaml/example1.ml)
- [Haskell](/src/Functional/Haskell/example1.hs)
- [Perl](/src/Scripting/Perl/example1.pl)
- [Python (PY)](/src/Scripting/PythonPY/example1.py)
- [Ruby (RB)](/src/Scripting/RubyRB/example1.rb)
- [Lua](/src/Scripting/Lua/example1.lua)
- [PHP](/src/Scripting/PHP/example1.php)
- [Groovy](/src/Scripting/Groovy/example1.groovy)
- [C](/src/Systems/C/example1.c)
- [C++](/src/Systems/C++/example1.cpp)
- [Rust](/src/Systems/Rust/example1.rs)
- [Zig](/src/Systems/Zig/example1.zig)
- [Go](/src/Systems/Go/example1.go)
- [Swift](/src/Systems/Swift/example1.swift)
- [Objective-C](/src/Systems/Objective-C/example1.m)
- [Java](/src/Enterprise/Java/example1.java)
- [C#](/src/Enterprise/CSharp/example1.cs)
- [Kotlin](/src/Enterprise/Kotlin/example1.kt)
- [Delphi](/src/Enterprise/Delphi/example1.pas)
- [Visual Basic](/src/Enterprise/VisualBasic/example1.vb)
- [R](/src/DataScience/R/example1.R)
- [Julia](/src/DataScience/Julia/example1.jl)
- [MATLAB](/src/DataScience/MATLAB/example1.m)
- [HTML/CSS](/src/Web/HTMLCSS/example1.html)
- [JavaScript (JS)](/src/Web/JavaScriptJS/example1.js)
- [TypeScript (TS)](/src/Web/TypeScriptTS/example1.ts)
- [Dart](/src/Web/Dart/example1.dart)
- [SQL](#) *(No disponible)*
- [Bash/Shell](/src/Shell/BashShell/example1.sh)
- [PowerShell](/src/Shell/PowerShell/example1.ps1)
- [VBA](/src/Shell/VBA/example1.bas)
- [Fortran](/src/Historical/Fortran/example1.f90)
- [Cobol](/src/Historical/Cobol/example1.cbl)
- [Ada](/src/Historical/Ada/example1.adb)
- [Solidity](/src/Niche/Solidity/example1.sol)
- [Prolog](/src/Niche/Prolog/example1.pl)
- [Nim](/src/Niche/Nim/example1.nim)
- [Crystal](/src/Niche/Crystal/example1.cr)
- [GDScript](/src/Niche/GDScript/example1.gd)
- [Assembly](/src/LowLevel/Assembly/example1.asm)
- [MicroPython](/src/Other/MicroPython/example1.py)
- [Rockstar](/src/Other/Rockstar/example1.rock)



### Caso 2: Sistema de bases de datos para múltiples plataformas

#### Explicación

En un sistema que necesita interactuar con diferentes bases de datos (por ejemplo, MySQL, PostgreSQL, SQLite), es útil utilizar el patrón Abstract Factory para crear conexiones y consultas específicas de cada plataforma sin acoplar la lógica de la aplicación a las clases concretas de las bases de datos.

Por ejemplo, podemos tener una fábrica abstracta `DatabaseFactory` con métodos para crear conexiones y consultas. Las fábricas concretas `MySQLFactory`, `PostgreSQLFactory` y `SQLiteFactory` implementan estos métodos para crear componentes específicos de cada base de datos.

#### Listado de Lenguajes

- [Erlang](/src/Functional/Erlang/example2.erl)
- [Elixir](/src/Functional/Elixir/example2.exs)
- [Clojure](/src/Functional/Clojure/example2.clj)
- [Scala](/src/Functional/Scala/example2.scala)
- [F#](/src/Functional/FSharp/example2.fsx)
- [Lisp](/src/Functional/Lisp/example2.lisp)
- [OCaml](/src/Functional/OCaml/example2.ml)
- [Haskell](/src/Functional/Haskell/example2.hs)
- [Perl](/src/Scripting/Perl/example2.pl)
- [Python (PY)](/src/Scripting/PythonPY/example2.py)
- [Ruby (RB)](/src/Scripting/RubyRB/example2.rb)
- [Lua](/src/Scripting/Lua/example2.lua)
- [PHP](/src/Scripting/PHP/example2.php)
- [Groovy](/src/Scripting/Groovy/example2.groovy)
- [C](/src/Systems/C/example2.c)
- [C++](/src/Systems/C++/example2.cpp)
- [Rust](/src/Systems/Rust/example2.rs)
- [Zig](/src/Systems/Zig/example2.zig)
- [Go](/src/Systems/Go/example2.go)
- [Swift](/src/Systems/Swift/example2.swift)
- [Objective-C](/src/Systems/Objective-C/example2.m)
- [Java](/src/Enterprise/Java/example2.java)
- [C#](/src/Enterprise/CSharp/example2.cs)
- [Kotlin](/src/Enterprise/Kotlin/example2.kt)
- [Delphi](/src/Enterprise/Delphi/example2.pas)
- [Visual Basic](/src/Enterprise/VisualBasic/example2.vb)
- [R](/src/DataScience/R/example2.R)
- [Julia](/src/DataScience/Julia/example2.jl)
- [MATLAB](/src/DataScience/MATLAB/example2.m)
- [HTML/CSS](/src/Web/HTMLCSS/example2.html)
- [JavaScript (JS)](/src/Web/JavaScriptJS/example2.js)
- [TypeScript (TS)](/src/Web/TypeScriptTS/example2.ts)
- [Dart](/src/Web/Dart/example2.dart)
- [SQL](/src/Databases/SQL/example2.sql) *(No disponible)*
- [Bash/Shell](/src/Shell/BashShell/example2.sh)
- [PowerShell](/src/Shell/PowerShell/example2.ps1)
- [VBA](/src/Shell/VBA/example2.bas)
- [Fortran](/src/Historical/Fortran/example2.f90)
- [Cobol](/src/Historical/Cobol/example2.cbl)
- [Ada](/src/Historical/Ada/example2.adb)
- [Solidity](/src/Niche/Solidity/example2.sol)
- [Prolog](/src/Niche/Prolog/example2.pl)
- [Nim](/src/Niche/Nim/example2.nim)
- [Crystal](/src/Niche/Crystal/example2.cr)
- [GDScript](/src/Niche/GDScript/example2.gd)
- [Assembly](/src/LowLevel/Assembly/example2.asm)
- [MicroPython](/src/Other/MicroPython/example2.py)
- [Rockstar](/src/Other/Rockstar/example2.rock)

### Caso 3: Generador de informes con diferentes formatos

#### Explicación

En una aplicación que necesita generar informes en múltiples formatos (por ejemplo, PDF, Word, Excel), se puede usar el patrón Abstract Factory para crear generadores de informes específicos de cada formato sin acoplar la lógica de la aplicación a las clases concretas de los generadores.

Por ejemplo, podemos tener una fábrica abstracta `ReportFactory` con métodos para crear informes en diferentes formatos. Las fábricas concretas `PDFReportFactory`, `WordReportFactory` y `ExcelReportFactory` implementan estos métodos para crear informes en los formatos correspondientes.

#### Listado de Lenguajes

- [Erlang](/src/Functional/Erlang/example3.erl)
- [Elixir](/src/Functional/Elixir/example3.exs)
- [Clojure](/src/Functional/Clojure/example3.clj)
- [Scala](/src/Functional/Scala/example3.scala)
- [F#](/src/Functional/FSharp/example3.fsx)
- [Lisp](/src/Functional/Lisp/example3.lisp)
- [OCaml](/src/Functional/OCaml/example3.ml)
- [Haskell](/src/Functional/Haskell/example3.hs)
- [Perl](/src/Scripting/Perl/example3.pl)
- [Python (PY)](/src/Scripting/PythonPY/example3.py)
- [Ruby (RB)](/src/Scripting/RubyRB/example3.rb)
- [Lua](/src/Scripting/Lua/example3.lua)
- [PHP](/src/Scripting/PHP/example3.php)
- [Groovy](/src/Scripting/Groovy/example3.groovy)
- [C](/src/Systems/C/example3.c)
- [C++](/src/Systems/C++/example3.cpp)
- [Rust](/src/Systems/Rust/example3.rs)
- [Zig](/src/Systems/Zig/example3.zig)
- [Go](/src/Systems/Go/example3.go)
- [Swift](/src/Systems/Swift/example3.swift)
- [Objective-C](/src/Systems/Objective-C/example3.m)
- [Java](/src/Enterprise/Java/example3.java)
- [C#](/src/Enterprise/CSharp/example3.cs)
- [Kotlin](/src/Enterprise/Kotlin/example3.kt)
- [Delphi](/src/Enterprise/Delphi/example3.pas)
- [Visual Basic](/src/Enterprise/VisualBasic/example3.vb)
- [R](/src/DataScience/R/example3.R)
- [Julia](/src/DataScience/Julia/example3.jl)
- [MATLAB](/src/DataScience/MATLAB/example3.m)
- [HTML/CSS](/src/Web/HTMLCSS/example3.html)
- [JavaScript (JS)](/src/Web/JavaScriptJS/example3.js)
- [TypeScript (TS)](/src/Web/TypeScriptTS/example3.ts)
- [Dart](/src/Web/Dart/example3.dart)
- [SQL](#) *(No disponible)*
- [Bash/Shell](/src/Shell/BashShell/example3.sh)
- [PowerShell](/src/Shell/PowerShell/example3.ps1)
- [VBA](/src/Shell/VBA/example3.bas)
- [Fortran](/src/Historical/Fortran/example3.f90)
- [Cobol](/src/Historical/Cobol/example3.cbl)
- [Ada](/src/Historical/Ada/example3.adb)
- [Solidity](/src/Niche/Solidity/example3.sol)
- [Prolog](/src/Niche/Prolog/example3.pl)
- [Nim](/src/Niche/Nim/example3.nim)
- [Crystal](/src/Niche/Crystal/example3.cr)
- [GDScript](/src/Niche/GDScript/example3.gd)
- [Assembly](/src/LowLevel/Assembly/example3.asm)
- [MicroPython](/src/Other/MicroPython/example3.py)
- [Rockstar](/src/Other/Rockstar/example3.rock)


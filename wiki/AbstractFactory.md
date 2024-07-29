# Abstract Factory Pattern

## Introducción

El patrón Abstract Factory es uno de los patrones de diseño creacionales que proporciona una interfaz para crear familias de objetos relacionados o dependientes sin especificar sus clases concretas. Es útil cuando un sistema debe ser independiente de cómo se crean, componen y representan sus objetos.

## Explicación del problema que resuelve

El patrón Abstract Factory resuelve el problema de la creación de objetos que pertenecen a diferentes familias sin acoplar el código cliente a las clases concretas de esos objetos. Este patrón es ideal en situaciones donde el sistema debe ser independiente de los productos que va a usar, y es necesario que sea fácil cambiar las familias de productos en el futuro. Por ejemplo, en una aplicación de interfaz de usuario que debe soportar múltiples temas (como oscuro y claro), Abstract Factory permite cambiar el tema sin modificar el código cliente.

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

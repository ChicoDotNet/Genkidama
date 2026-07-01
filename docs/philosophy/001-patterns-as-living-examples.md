# Patterns as Living Examples

Genkidama should preserve its educational origin without becoming an artificial pattern showcase.

The project should include practical examples of the classic design patterns commonly taught from the Gang of Four catalog and Head First Design Patterns.

The goal is not to force every pattern into the production architecture.

The goal is to show where each pattern naturally improves clarity, extensibility, testability, or replacement of infrastructure.

## Rule

Use patterns when they make the generated application easier to understand, extend, or operate.

Do not use patterns only to prove that the pattern exists.

## Examples

- Factory Method: database provider factory and module creation.
- Abstract Factory: persistence provider families.
- Strategy: notification channels and authentication modes.
- Adapter: external integrations and provider-specific clients.
- Facade: generated application service APIs.
- Observer: StandardEvent and notification dispatch.
- Command: command handlers and CLI operations.
- Template Method: scaffold generation steps.
- Builder: project generation configuration.
- Repository: persistence access abstraction.
- Unit of Work: transactional boundaries.
- Decorator: logging, caching, validation, and authorization wrappers.
- Proxy: backend API clients and remote service clients.

## Documentation expectation

When a pattern is used deliberately, the example should document:

- The pattern name.
- The problem being solved.
- Why the pattern is useful here.
- Why a simpler approach was not enough.
- How to recognize the pattern in the generated code.

## Constraint

The architecture comes first.

The pattern catalog supports the architecture; it must not distort it.

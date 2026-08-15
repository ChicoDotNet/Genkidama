# Genkidama

Genkidama is an open source .NET CLI and application scaffold for building modern, contract-first applications.

It also preserves its educational origin through a reference Example App: a multi-language Design Patterns catalog that demonstrates classic software design patterns in practical generated applications.

## Available for collaboration

I'm actively looking for opportunities to collaborate on modern .NET, Azure, React, Angular, AI-assisted software development, software architecture, and enterprise modernization projects.

If you believe Genkidama or my engineering experience can help your organization, I'd be glad to talk.

Contact: alfonso.lara.ramos.dev@outlook.com

## Vision

Genkidama should help creators of solutions start faster without sacrificing architecture.

The CLI generates production-oriented application foundations.

The Example App teaches the architecture by showing real patterns, real clients, and real backend boundaries.

## Repository purpose

This repository has two complementary purposes:

1. **Genkidama CLI**: a .NET tool that generates application scaffolds, modules, contracts, clients, and vertical slices.
2. **Genkidama Example App**: an educational Design Patterns application used to demonstrate the generated architecture across multiple languages and client technologies.

The former makes Genkidama useful in real projects.

The latter makes Genkidama understandable, teachable, and extensible.

## Branch strategy

- `genkidama-cli`: feature work for the CLI and generated templates.
- `dev`: integration branch.
- `main`: stable public branch.

## Core architecture

Generated applications follow a contract-first architecture.

The baseline backend includes:

- API endpoints or controllers
- Contracts
- Application services
- Command and query handlers
- Repositories
- Unit of Work
- DbContext
- Domain model
- StandardResult
- StandardQuery
- StandardJob
- StandardEvent
- Notifications
- Audit trail
- Security context
- Typed API clients

## Supported database providers

The core persistence factory supports only:

- MariaDB
- SQLite
- SQL Server
- PostgreSQL

Additional providers, such as DB2, Oracle, Cosmos DB, or other engines, should be added through plugins.

## Example App: Design Patterns Catalog

The original educational material in this repository becomes the first canonical Genkidama Example App.

The Example App should eventually expose a UI that lets users explore classic design patterns, inspect generated code, and compare implementations across languages.

It should cover patterns commonly associated with the Gang of Four catalog and Head First Design Patterns, without forcing patterns where they do not improve the architecture.

Design patterns rarely appear in isolation. The navigable catalog now lives in the [Design Patterns wiki index](wiki/README.md), which groups every current pattern page by family and adds a Mermaid relationship map showing common collaborations, alternatives, and conceptual relatives.

Use that index when choosing, comparing, or combining patterns; use each individual page for pattern-specific explanations and examples.

## Example language targets

The educational catalog may include examples in:

- Functional languages: Erlang, Elixir, Clojure, Scala, F#, Lisp, OCaml, Haskell.
- Scripting languages: Perl, Python, Ruby, Lua, PHP, Groovy.
- Systems languages: C, C++, Rust, Zig, Go, Swift, Objective-C.
- Enterprise languages: Java, C#, Kotlin, Delphi, Visual Basic.
- Data science languages: R, Julia, GNU Octave.
- Web languages: HTML/CSS, JavaScript, TypeScript, Dart.
- Databases: SQL.
- Shell languages: Bash, PowerShell, VBA.
- Historical languages: Fortran, Cobol, Ada.
- Niche languages: Solidity, Prolog, Nim, Crystal, GDScript.
- Low-level languages: Assembly.
- Other languages: MicroPython, Rockstar.

## Genkidama Learn

[Genkidama Learn](learn/README.md) extends the educational side of the repository with practical, application-centered courses designed to take a learner from zero knowledge of a language to a reasonable Junior Developer / Entry Level foundation.

The canonical source is initially Spanish, with 45 current language courses planned for v1 and five pilots used to stabilize the format before scaling.

## Documentation

Important project documents live under `docs/` and the pattern catalog under `wiki/`.

- [Design Patterns catalog and relationship map](wiki/README.md)
- `docs/engineering-standards.md`
- `docs/roadmap.md`
- `docs/philosophy/001-patterns-as-living-examples.md`

Pattern pages under `wiki/` are progressively evolving from placeholders into full explanations and cross-links to the Example App. The wiki index provides stable navigation while that content matures.

## Quality gates

MSTest is mandatory from the bootstrap delivery.

Minimum global coverage is 44%.

The ideal coverage target is greater than 72.8%.

All public and internal C# APIs require XML documentation in English.

## License

Genkidama is open source under the MIT License.

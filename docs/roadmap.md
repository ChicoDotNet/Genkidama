# Genkidama Roadmap

This document is the authoritative implementation roadmap for Genkidama.

## Branch strategy

- Feature work: genkidama-cli
- Integration: dev
- Stable releases: main

## Program model

Genkidama moves through two coordinated pods.

The Engineering Core pod builds the CLI, generated architecture, templates and runtime conventions.

The Knowledge Base pod captures the engineering standards, skills, architectural decisions, examples and educational material that guide the generated output.

The desired flow is:

```text
Knowledge Base -> Templates -> CLI -> Generated Applications
```

The knowledge should govern the generated code, not the other way around.

## Pod A: Engineering Core

### GEN-000
Repository foundation, branch setup, license, governance documents and repository purpose.

### GEN-001
CLI bootstrap, engineering standards, MSTest, CI foundation, roadmap and documentation structure.

### GEN-002
`genkidama new` solution generator.

### GEN-003
StandardResult, StandardCollectionResult, StandardQuery, StandardProblem.

### GEN-004
HTTP layer, HttpQuery attribute, middleware, trace identifiers.

### GEN-005
Persistence factory, supported providers (MariaDB, SQLite, SQL Server, PostgreSQL), schema separation.

### GEN-006
Repository and Unit of Work.

### GEN-007
Command/Query pipeline.

### GEN-008
StandardJob and background processing.

### GEN-009
StandardEvent and notification pipeline.

### GEN-010
Security foundation.

### GEN-011
React reference client and StandardResult API client.

### GEN-012
Angular reference client and StandardResult API client.

### GEN-013
Console reference client and typed backend client.

### GEN-014
WinForms MVP reference client.

### GEN-015
MAUI MVVM reference client.

### GEN-016
`genkidama add entity` generator.

### GEN-017
`genkidama add enum` generator and Enum schema seed support.

### GEN-018
`genkidama add feature` vertical slice generator.

### GEN-019
`genkidama add module` generator for audit, jobs, notifications, storage, security and integrations.

### GEN-020
Template system, plugins and provider extension points.

### GEN-021
Testing hardening, contract tests, integration tests and coverage reporting.

### GEN-022
Packaging, .NET Tool publication and release automation.

### GEN-023
Localization foundation.

Scope:

- Add Contract.Constants for stable localization keys.
- Add .resx resource structure for generated .NET applications.
- Define supported locales: en, es, fr, de, it, pt, ja and zh-Hans.
- Keep generated source code, namespaces, public members and XML documentation in English.
- Localize user-facing documentation, Example App educational content, UI labels and generated README files.

### GEN-024
Design Patterns Example App localization.

Scope:

- Localize pattern explanations in English, Spanish, French, German, Italian, Portuguese, Japanese and Simplified Chinese.
- Keep pattern identifiers stable through Contract.Constants.
- Use .resx for UI and server-rendered/generated text where appropriate.
- Allow the Example App to switch language without changing the underlying code examples.

### GEN-025
Documentation localization pipeline.

Scope:

- Define source documentation structure.
- Generate localized documentation folders.
- Add review workflow for translated documentation.
- Ensure localized docs do not drift from the authoritative source.

## Pod B: Knowledge Base

### KB-000
Engineering Bible foundation.

### KB-001
Engineering standards consolidation.

### KB-002
Architecture reference.

### KB-003
Architecture Decision Records.

### KB-004
Design principles and philosophy.

### KB-005
C# and .NET skill standard.

### KB-006
React skill standard.

### KB-007
Angular skill standard.

### KB-008
MAUI skill standard.

### KB-009
WinForms skill standard.

### KB-010
Console application skill standard.

### KB-011
SQL skill standard.

### KB-012
SQLite skill standard.

### KB-013
PostgreSQL skill standard.

### KB-014
SQL Server skill standard.

### KB-015
MariaDB skill standard.

### KB-016
REST, HttpQuery and StandardResult reference.

### KB-017
StandardQuery reference.

### KB-018
StandardEvent and notification reference.

### KB-019
Plugin system reference.

### KB-020
Localization reference.

### KB-021
Design Patterns educational catalog.

### KB-022
Example App reference.

### KB-023
Documentation standard.

### KB-024
Translation pipeline.

### KB-025
Release playbook.

## Definition of Done

- Builds successfully when code exists.
- MSTest passes when tests exist.
- Coverage >= 44% when a testable codebase exists.
- XML documentation is updated for public and internal C# APIs.
- Engineering standards are respected.
- Roadmap is updated when scope changes.
- New user-facing text uses localization keys when applicable.
- Knowledge Base impact is considered for template or architecture changes.

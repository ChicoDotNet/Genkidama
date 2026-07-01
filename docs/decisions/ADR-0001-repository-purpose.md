# ADR-0001: Repository Purpose

## Status

Accepted.

## Context

Genkidama started as an educational Design Patterns tutorial placeholder with wiki pages and multi-language examples.

The project is now evolving into an open source .NET CLI and application scaffold that generates modern, contract-first applications.

Both directions are valuable.

The CLI makes Genkidama useful in real projects.

The educational material makes Genkidama teachable and explains the architectural choices behind the generated code.

## Decision

This repository will contain both the Genkidama CLI and the Genkidama Example App.

The Example App is the canonical demonstration application for the framework.

## Consequences

- The repository remains public and open source.
- The project uses the MIT License.
- The CLI is written in .NET.
- Generated applications follow a contract-first architecture.
- The Design Patterns material becomes an educational UI and documentation surface.
- Code and XML documentation remain in English.
- Educational content may be localized into English, Spanish, French, German, Italian, Portuguese, Japanese and Simplified Chinese.

## Non-goals

- The project will not force design patterns where they do not improve the architecture.
- The project will not move the educational material to a separate repository during this stage.

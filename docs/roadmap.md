# Genkidama Roadmap

This document is the authoritative implementation roadmap for Genkidama.

## Branch strategy
- Feature work: genkidama-cli
- Integration: dev
- Stable releases: main

## GEN-000
Repository preparation, backup and migration.

## GEN-001
CLI bootstrap, engineering standards, MSTest, CI foundation.

## GEN-002
`genkidama new` solution generator.

## GEN-003
StandardResult, StandardCollectionResult, StandardQuery, StandardProblem.

## GEN-004
HTTP layer, HttpQuery attribute, middleware, trace identifiers.

## GEN-005
Persistence factory, supported providers (MariaDB, SQLite, SQL Server, PostgreSQL), schema separation.

## GEN-006
Repository and Unit of Work.

## GEN-007
Command/Query pipeline.

## GEN-008
StandardJob and background processing.

## GEN-009
StandardEvent and notification pipeline.

## GEN-010
Security foundation.

## GEN-011 through GEN-015
Reference clients: React, Angular, MAUI, WinForms and Console.

## GEN-016 through GEN-020
Scaffolding generators, modules, templates and plugins.

## GEN-021
Testing hardening.

## GEN-022
Packaging, .NET Tool publication and release automation.

## Definition of Done
- Builds successfully.
- MSTest passes.
- Coverage >= 44%.
- XML documentation updated.
- Engineering standards respected.
- Roadmap updated when scope changes.

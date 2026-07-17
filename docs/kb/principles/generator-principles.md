# Generator Principles

These principles guide blueprints and CLI generators.

## Prefer explicit generated files

Generated output should be easy to inspect. Prefer clear files and folders over hidden conventions.

## Generate the smallest useful slice

A generator should create enough structure to be useful without pretending to finish design decisions that belong to a later delivery.

## Keep names stable

Generated file paths, type names and command names should be stable unless an architecture decision explains the change.

## Avoid premature wiring

Do not force dependency injection, persistence, background processing or client behavior before the relevant architecture rule exists.

## Favor owned abstractions

When the generated architecture needs an extension point, prefer a Genkidama-owned abstraction first. External packages can be added later as optional adapters.

## Document intentional gaps

If a generator leaves a handler, registration or integration intentionally small, the delivery notes should say why.

## Keep local restore healthy

Any generated or repository project that participates in the active build should have solution membership reviewed.

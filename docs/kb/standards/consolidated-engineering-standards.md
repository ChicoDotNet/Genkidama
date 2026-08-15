# Consolidated Engineering Standards

KB-001 consolidates the active engineering standards into a single operational reference.

## Delivery standards

Every delivery should:

- Start from the current integration branch unless the user explicitly requests a stacked branch.
- Keep changes small enough for focused review.
- Add or update delivery notes under `docs/deliveries`.
- Avoid new runtime dependencies unless the delivery explicitly requires them.
- Keep generated application intent visible in documentation, not only in source code.

## Code standards

C# code should:

- Use nullable reference types.
- Prefer small methods with direct control flow.
- Keep public and internal APIs documented in English XML comments.
- Keep domain, application, contract and infrastructure concerns separated.
- Favor platform primitives before introducing external libraries.

## Test standards

Tests should:

- Use MSTest.
- Be deterministic and isolated.
- Use temporary workspaces for file generation.
- Assert behavior rather than implementation details when possible.
- Keep the existing CI path green before adding new validation steps.

## Documentation standards

Documentation should:

- State the purpose of each delivery.
- Describe generated files when a generator is changed.
- Explain intentional gaps and deferred work.
- Prefer stable headings and paths so future localization can reference them.

## Generated application standards

Generated applications should:

- Use contracts for external boundaries.
- Return predictable result shapes.
- Keep persistence providers replaceable.
- Keep clients aligned with the same contract model.
- Keep user-facing text ready for localization keys.

## Knowledge Base rule

When code generation behavior changes, the Knowledge Base should be checked first. The standard should guide the blueprint and CLI behavior, not the other way around.

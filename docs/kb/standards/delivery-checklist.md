# Delivery Checklist

Use this checklist before opening a delivery pull request.

## Branch and scope

- The branch starts from the intended base.
- The delivery scope matches the roadmap item.
- Manual changes in the base branch have not been skipped.
- Deferred work is named explicitly.

## Build and tests

- Existing CI should still restore, build and test.
- New code has MSTest coverage when it is testable.
- File generation tests use isolated temporary folders.
- Behavior tests are preferred over fragile implementation inspections.

## Code quality

- Public and internal APIs include XML documentation.
- Namespaces and public member names are in English.
- Methods remain small and direct.
- New dependencies are avoided unless justified.

## Generated output

- Generated paths are predictable.
- Generated source code compiles in a normal generated project.
- Generated user-facing text is ready for future localization.
- The generated architecture remains aligned with contracts and result models.

## Documentation

- Delivery notes exist under `docs/deliveries`.
- Knowledge Base impact has been considered.
- Any connector-driven naming workaround is documented honestly.
- The PR description can be minimal if connector controls block richer text, but the repository documentation must carry the full explanation.

# KB-006 RED audit

> **Iteration:** 1  
> **State:** RED baseline  
> **Purpose:** capture the executable acceptance contract before changing incomplete pattern pages.

This file records the first TDD-style documentation iteration for KB-006. The audit implemented in this branch is intentionally expected to report current catalog debt. Pattern pages are not rewritten in this iteration.

## Acceptance contract

The executable audit must report, at minimum:

- every catalog entry and whether its target page exists;
- zero-byte or effectively empty pattern pages;
- required KB-006 canonical sections missing from non-empty pages;
- unresolved relative Markdown links inside `wiki/`;
- implementation links that do not resolve to repository paths;
- forbidden debt markers (`TODO`, `TBD`, `PLACEHOLDER`) in pages claiming `validated`;
- `validated` pages whose `implemented/applicable` counters are inconsistent;
- the aggregate catalog census required to plan the GREEN iterations.

## RED rule

A non-zero audit result is evidence, not a reason to weaken the standard. The next iteration repays the measured debt; it does not silence or relax the checks.

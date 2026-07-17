# Design Philosophy

Genkidama exists to make high-quality application architecture easier to generate, teach, review and evolve.

## Principles

### Knowledge before generation

Architectural intent should be documented before it is hidden inside blueprints or CLI behavior.

### Simple core, optional edges

The core should remain small, clear and useful without forcing every generated application to carry optional providers or adapters.

### Teach by structure

Generated projects should teach the architecture through names, folders, contracts and examples. A developer should learn the intended design by reading the generated solution.

### Production-shaped examples

Reference clients and generated applications should be simple enough to learn from, but shaped like real production work rather than disposable demos.

### Replaceable infrastructure

Generated applications should make persistence, background work, access control, events and integrations replaceable without changing the business model.

### Local developer experience matters

CI passing is not enough. Visual Studio, command line restore and day-to-day local development should also remain healthy.

### Documentation is part of architecture

When documentation is missing, the architecture is incomplete. The Knowledge Base should be treated as a design artifact, not as after-the-fact notes.

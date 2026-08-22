from __future__ import annotations

from copy import deepcopy
from dataclasses import dataclass, field
from typing import Protocol, TypeVar

T = TypeVar("T", covariant=True)


class Prototype(Protocol[T]):
    def clone(self) -> T: ...


@dataclass
class ServiceProfile:
    name: str
    features: list[str] = field(default_factory=list)

    def clone(self) -> ServiceProfile:
        return deepcopy(self)

    def describe(self) -> str:
        return f"{self.name}: {','.join(self.features)}"


def main() -> None:
    prototype: Prototype[ServiceProfile] = ServiceProfile("orders", ["metrics"])
    original = prototype
    canary = prototype.clone()

    canary.name = "orders-canary"
    canary.features.append("tracing")

    print(f"original={original.describe()}")
    print(f"clone={canary.describe()}")


if __name__ == "__main__":
    main()

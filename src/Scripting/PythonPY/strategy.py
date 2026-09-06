"""Canonical Python example for the Strategy pattern."""
from __future__ import annotations

from collections.abc import Callable, Sequence
from typing import TypeVar

T = TypeVar("T")


def choose(values: Sequence[T], strategy: Callable[[Sequence[T]], T]) -> T:
    """Delegate the selection algorithm to an interchangeable strategy."""
    return strategy(values)


def verify() -> None:
    values = [3, 1, 2]

    assert choose(values, min) == 1
    assert choose(values, max) == 3
    assert choose(values, lambda items: items[1]) == 1


if __name__ == "__main__":
    verify()
    print("python-strategy: passed")

"""Canonical State pattern example for Python."""
from __future__ import annotations

from typing import Protocol


class TurnstileState(Protocol):
    name: str

    def insert_coin(self, turnstile: "Turnstile") -> str: ...

    def push(self, turnstile: "Turnstile") -> str: ...


class Locked:
    name = "locked"

    def insert_coin(self, turnstile: "Turnstile") -> str:
        turnstile.state = Unlocked()
        return "unlocked"

    def push(self, turnstile: "Turnstile") -> str:
        return "rejected"


class Unlocked:
    name = "unlocked"

    def insert_coin(self, turnstile: "Turnstile") -> str:
        return "already-unlocked"

    def push(self, turnstile: "Turnstile") -> str:
        turnstile.state = Locked()
        return "passed"


class Turnstile:
    def __init__(self) -> None:
        self.state: TurnstileState = Locked()

    def insert_coin(self) -> str:
        return self.state.insert_coin(self)

    def push(self) -> str:
        return self.state.push(self)


def verify() -> None:
    gate = Turnstile()
    assert gate.state.name == "locked"
    assert gate.push() == "rejected"
    assert gate.state.name == "locked"
    assert gate.insert_coin() == "unlocked"
    assert gate.state.name == "unlocked"
    assert gate.insert_coin() == "already-unlocked"
    assert gate.state.name == "unlocked"
    assert gate.push() == "passed"
    assert gate.state.name == "locked"
    print("python-state: passed")


if __name__ == "__main__":
    verify()

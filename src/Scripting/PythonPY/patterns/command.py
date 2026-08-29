"""Canonical Python Command example for KB-006."""

from dataclasses import dataclass


@dataclass(frozen=True)
class Command:
    operation: str
    amount: int


def execute(balance: int, command: Command) -> int:
    if command.operation == "deposit":
        return balance + command.amount
    if command.operation == "withdraw":
        return balance - command.amount
    raise ValueError(f"unknown command: {command.operation}")


def main() -> None:
    queue = [Command("deposit", 50), Command("withdraw", 20)]
    balance = 100
    for command in queue:
        balance = execute(balance, command)
    assert balance == 130
    assert len(queue) == 2
    print(f"balance={balance};commands={len(queue)}")


if __name__ == "__main__":
    main()

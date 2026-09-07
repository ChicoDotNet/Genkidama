"""Canonical Python example for the Mediator design pattern."""

from __future__ import annotations

from collections.abc import Callable

Receiver = Callable[[str, str], str]


class CheckoutMediator:
    """Owns colleague registration and all colleague-to-colleague routing."""

    def __init__(self) -> None:
        self._colleagues: dict[str, Receiver] = {}

    def register(self, name: str, receiver: Receiver) -> None:
        self._colleagues[name] = receiver

    def send(self, sender: str, recipient: str, message: str) -> str:
        try:
            receiver = self._colleagues[recipient]
        except KeyError as error:
            raise ValueError(f"unknown colleague: {recipient}") from error
        return receiver(sender, message)


def verify_mediator() -> None:
    mediator = CheckoutMediator()
    mediator.register(
        "payment",
        lambda sender, message: f"payment<-{sender}:{message}",
    )
    mediator.register(
        "inventory",
        lambda sender, message: f"inventory<-{sender}:{message}",
    )

    assert (
        mediator.send("payment", "inventory", "reserve-order-42")
        == "inventory<-payment:reserve-order-42"
    )
    assert (
        mediator.send("inventory", "payment", "reserved-order-42")
        == "payment<-inventory:reserved-order-42"
    )

    try:
        mediator.send("payment", "shipping", "dispatch-order-42")
    except ValueError as error:
        assert str(error) == "unknown colleague: shipping"
    else:
        raise AssertionError("Mediator must reject an unknown colleague")


if __name__ == "__main__":
    verify_mediator()
    print("python-mediator: passed")

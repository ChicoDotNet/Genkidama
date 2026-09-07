"""Canonical Observer example for Genkidama.

The subject owns subscription lifecycle but knows observers only as callables.
"""
from __future__ import annotations

from collections.abc import Callable

Observer = Callable[[str], None]


class Document:
    def __init__(self) -> None:
        self._observers: list[Observer] = []
        self.state = "draft"

    def subscribe(self, observer: Observer) -> None:
        if observer not in self._observers:
            self._observers.append(observer)

    def unsubscribe(self, observer: Observer) -> None:
        self._observers.remove(observer)

    def publish(self) -> None:
        self.state = "published"
        for observer in tuple(self._observers):
            observer(self.state)


def verify_observer_canonical() -> None:
    audit: list[str] = []
    dashboard: list[str] = []
    document = Document()

    audit_observer = lambda state: audit.append(f"audit:{state}")
    dashboard_observer = lambda state: dashboard.append(f"dashboard:{state}")

    document.subscribe(audit_observer)
    document.subscribe(dashboard_observer)
    document.publish()

    assert audit == ["audit:published"]
    assert dashboard == ["dashboard:published"]

    document.unsubscribe(dashboard_observer)
    document.publish()

    assert audit == ["audit:published", "audit:published"]
    assert dashboard == ["dashboard:published"]


if __name__ == "__main__":
    verify_observer_canonical()
    print("Python Observer: passed")

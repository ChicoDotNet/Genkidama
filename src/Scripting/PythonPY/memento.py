"""Canonical Python Memento example for Genkidama.

The originator owns snapshot creation and restoration. The caretaker only stores
opaque snapshots and cannot mutate the originator's live state directly.
"""
from __future__ import annotations

from copy import deepcopy
from dataclasses import dataclass


@dataclass(frozen=True)
class DocumentMemento:
    title: str
    tags: tuple[str, ...]


class Document:
    def __init__(self, title: str, tags: list[str]) -> None:
        self._title = title
        self._tags = list(tags)

    @property
    def state(self) -> tuple[str, tuple[str, ...]]:
        return self._title, tuple(self._tags)

    def rename(self, title: str) -> None:
        self._title = title

    def add_tag(self, tag: str) -> None:
        self._tags.append(tag)

    def save(self) -> DocumentMemento:
        title, tags = deepcopy(self.state)
        return DocumentMemento(title, tags)

    def restore(self, snapshot: DocumentMemento) -> None:
        if not isinstance(snapshot, DocumentMemento):
            raise TypeError("snapshot must be a DocumentMemento")
        self._title = snapshot.title
        self._tags = list(snapshot.tags)


def verify_memento() -> None:
    document = Document("draft", ["pattern"])
    snapshot = document.save()

    document.rename("published")
    document.add_tag("edited")

    assert snapshot == DocumentMemento("draft", ("pattern",))
    assert document.state == ("published", ("pattern", "edited"))

    document.restore(snapshot)
    assert document.state == ("draft", ("pattern",))

    try:
        document.restore(object())  # type: ignore[arg-type]
    except TypeError as exc:
        assert str(exc) == "snapshot must be a DocumentMemento"
    else:
        raise AssertionError("invalid snapshots must fail explicitly")


if __name__ == "__main__":
    verify_memento()
    print("Python Memento: passed")

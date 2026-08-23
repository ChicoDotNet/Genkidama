from __future__ import annotations

from abc import ABC, abstractmethod


class Component(ABC):
    @abstractmethod
    def render(self) -> str:
        raise NotImplementedError


class PlainMessage(Component):
    def render(self) -> str:
        return "alert"


class ComponentDecorator(Component):
    def __init__(self, inner: Component) -> None:
        self._inner = inner


class AuditDecorator(ComponentDecorator):
    def render(self) -> str:
        return f"audit({self._inner.render()})"


class EncryptDecorator(ComponentDecorator):
    def render(self) -> str:
        return f"enc({self._inner.render()})"


def main() -> None:
    component: Component = PlainMessage()
    print(f"base={component.render()}")
    print(f"audit={AuditDecorator(component).render()}")
    print(f"encrypted={EncryptDecorator(component).render()}")
    print(f"stacked={AuditDecorator(EncryptDecorator(component)).render()}")


if __name__ == "__main__":
    main()

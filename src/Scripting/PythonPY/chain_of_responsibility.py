from __future__ import annotations


class RefundHandler:
    name = "base"

    def __init__(self) -> None:
        self._next: RefundHandler | None = None

    def set_next(self, next_handler: RefundHandler) -> RefundHandler:
        self._next = next_handler
        return next_handler

    def can_handle(self, amount: int) -> bool:
        raise NotImplementedError

    def handle(self, amount: int, visited: list[str]) -> str:
        visited.append(self.name)
        if self.can_handle(amount):
            return self.name
        if self._next is None:
            raise RuntimeError("No handler accepted the request.")
        return self._next.handle(amount, visited)


class FaqHandler(RefundHandler):
    name = "faq"

    def can_handle(self, amount: int) -> bool:
        return amount <= 50


class BillingHandler(RefundHandler):
    name = "billing"

    def can_handle(self, amount: int) -> bool:
        return amount <= 500


class EscalationHandler(RefundHandler):
    name = "escalation"

    def can_handle(self, amount: int) -> bool:
        return True


faq = FaqHandler()
billing = BillingHandler()
escalation = EscalationHandler()
faq.set_next(billing).set_next(escalation)
visited: list[str] = []
handled = faq.handle(250, visited)
print(f"visited={'>'.join(visited)};handled={handled};result=refund(250)")

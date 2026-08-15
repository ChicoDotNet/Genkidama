"""Pure collection-based analysis for LedgerMatch reconciliation results."""

from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal

from .models import MatchStatus
from .reconciler import ReconciliationLine, ReconciliationSummary


@dataclass(frozen=True, slots=True)
class CustomerSummary:
    """Aggregate accepted invoice/payment lines for one normalized customer."""

    customer: str
    invoice_count: int
    invoice_total: Decimal
    payment_total: Decimal

    @property
    def difference(self) -> Decimal:
        """Return payment total minus invoice total for this customer."""

        return self.payment_total - self.invoice_total


def summarize_by_customer(summary: ReconciliationSummary) -> tuple[CustomerSummary, ...]:
    """Group accepted reconciliation lines by customer without performing I/O.

    Customer matching is case-insensitive while the first accepted spelling is
    preserved for display. Results are returned in deterministic name order.
    """

    buckets: dict[str, list[ReconciliationLine]] = {}
    display_names: dict[str, str] = {}

    for line in summary.lines:
        key = line.record.customer.casefold()
        display_names.setdefault(key, line.record.customer)
        buckets.setdefault(key, []).append(line)

    return tuple(
        CustomerSummary(
            customer=display_names[key],
            invoice_count=len(buckets[key]),
            invoice_total=sum(
                (line.record.invoice_total for line in buckets[key]), Decimal("0")
            ),
            payment_total=sum(
                (line.record.payment_total for line in buckets[key]), Decimal("0")
            ),
        )
        for key in sorted(buckets, key=lambda item: display_names[item].casefold())
    )


def select_lines(
    summary: ReconciliationSummary,
    *,
    customer: str | None = None,
    only_differences: bool = False,
) -> tuple[ReconciliationLine, ...]:
    """Select reconciliation lines by optional customer and difference status."""

    customer_key = customer.strip().casefold() if customer is not None else None
    return tuple(
        line
        for line in summary.lines
        if (customer_key is None or line.record.customer.casefold() == customer_key)
        and (not only_differences or line.status is MatchStatus.DIFFERENCE)
    )

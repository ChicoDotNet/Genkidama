from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal

from .models import InvoiceRecord, MatchStatus
from .parser import ParseResult


@dataclass(frozen=True, slots=True)
class ReconciliationLine:
    record: InvoiceRecord
    status: MatchStatus
    difference: Decimal


@dataclass(frozen=True, slots=True)
class ReconciliationSummary:
    lines: tuple[ReconciliationLine, ...]
    invalid_rows: int

    @property
    def matched(self) -> int:
        return sum(line.status is MatchStatus.MATCHED for line in self.lines)

    @property
    def different(self) -> int:
        return sum(line.status is MatchStatus.DIFFERENCE for line in self.lines)

    @property
    def invoice_total(self) -> Decimal:
        return sum((line.record.invoice_total for line in self.lines), Decimal("0"))

    @property
    def payment_total(self) -> Decimal:
        return sum((line.record.payment_total for line in self.lines), Decimal("0"))


def reconcile(parsed: ParseResult) -> ReconciliationSummary:
    lines = tuple(
        ReconciliationLine(
            record=record,
            status=(
                MatchStatus.MATCHED
                if record.invoice_total == record.payment_total
                else MatchStatus.DIFFERENCE
            ),
            difference=record.payment_total - record.invoice_total,
        )
        for record in parsed.records
    )
    invalid_rows = len({issue.row_number for issue in parsed.issues})
    return ReconciliationSummary(lines=lines, invalid_rows=invalid_rows)

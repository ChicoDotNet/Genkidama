from __future__ import annotations

from dataclasses import dataclass
from datetime import date
from decimal import Decimal
from enum import StrEnum


class MatchStatus(StrEnum):
    MATCHED = "matched"
    DIFFERENCE = "difference"


@dataclass(frozen=True, slots=True)
class InvoiceRecord:
    invoice_id: str
    customer: str
    issued_on: date
    invoice_total: Decimal
    payment_total: Decimal


@dataclass(frozen=True, slots=True)
class ValidationIssue:
    row_number: int
    field: str
    message: str

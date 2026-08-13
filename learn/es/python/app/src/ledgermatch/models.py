"""Business data models used by LedgerMatch."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import date
from decimal import Decimal
from enum import StrEnum


class MatchStatus(StrEnum):
    """Describe whether one invoice and its payment reconcile exactly."""

    MATCHED = "matched"
    DIFFERENCE = "difference"


@dataclass(frozen=True, slots=True)
class InvoiceRecord:
    """Represent one validated invoice/payment row from the input file."""

    invoice_id: str
    customer: str
    issued_on: date
    invoice_total: Decimal
    payment_total: Decimal


@dataclass(frozen=True, slots=True)
class ValidationIssue:
    """Describe one actionable validation problem found in an input row."""

    row_number: int
    field: str
    message: str

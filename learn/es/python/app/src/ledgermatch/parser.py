"""Parse and validate LedgerMatch CSV input at the file boundary."""

from __future__ import annotations

import csv
from dataclasses import dataclass
from datetime import date
from decimal import Decimal, InvalidOperation
from pathlib import Path

from .models import InvoiceRecord, ValidationIssue

_REQUIRED_HEADERS = {"invoice_id", "customer", "issued_on", "invoice_total", "payment_total"}


class CsvSchemaError(ValueError):
    """Raised when the CSV cannot represent LedgerMatch input rows."""


@dataclass(frozen=True, slots=True)
class ParseResult:
    """Contain accepted records and row-level validation issues from one CSV."""

    records: tuple[InvoiceRecord, ...]
    issues: tuple[ValidationIssue, ...]


def _money(raw: str | None, *, row_number: int, field: str) -> tuple[Decimal | None, ValidationIssue | None]:
    text = (raw or "").strip()
    if not text:
        return None, ValidationIssue(row_number, field, "El importe es obligatorio.")
    try:
        value = Decimal(text)
    except InvalidOperation:
        return None, ValidationIssue(row_number, field, "El importe debe ser decimal.")
    if not value.is_finite() or value < 0:
        return None, ValidationIssue(row_number, field, "El importe debe ser finito y no negativo.")
    return value, None


def read_invoices(path: str | Path) -> ParseResult:
    """Read, validate and normalize invoice/payment rows from a CSV file.

    Args:
        path: CSV file path. UTF-8 with an optional BOM is accepted.

    Returns:
        Accepted records plus row-level validation issues. Invalid rows are not
        included in ``records`` and input order is preserved.

    Raises:
        FileNotFoundError: The requested file does not exist.
        CsvSchemaError: One or more required columns are missing.
    """

    source = Path(path)
    records: list[InvoiceRecord] = []
    issues: list[ValidationIssue] = []
    seen_invoice_ids: set[str] = set()

    with source.open("r", encoding="utf-8-sig", newline="") as stream:
        reader = csv.DictReader(stream)
        headers = set(reader.fieldnames or ())
        missing = sorted(_REQUIRED_HEADERS - headers)
        if missing:
            raise CsvSchemaError(f"Faltan columnas obligatorias: {', '.join(missing)}")

        for row_number, row in enumerate(reader, start=2):
            row_issues: list[ValidationIssue] = []
            invoice_id = (row.get("invoice_id") or "").strip()
            customer = (row.get("customer") or "").strip()
            if not invoice_id:
                row_issues.append(ValidationIssue(row_number, "invoice_id", "El identificador es obligatorio."))
            elif invoice_id in seen_invoice_ids:
                row_issues.append(ValidationIssue(row_number, "invoice_id", "El identificador está duplicado."))
            if not customer:
                row_issues.append(ValidationIssue(row_number, "customer", "El cliente es obligatorio."))

            issued_on: date | None = None
            try:
                issued_on = date.fromisoformat((row.get("issued_on") or "").strip())
            except ValueError:
                row_issues.append(ValidationIssue(row_number, "issued_on", "La fecha debe usar YYYY-MM-DD."))

            invoice_total, invoice_issue = _money(row.get("invoice_total"), row_number=row_number, field="invoice_total")
            payment_total, payment_issue = _money(row.get("payment_total"), row_number=row_number, field="payment_total")
            if invoice_issue:
                row_issues.append(invoice_issue)
            if payment_issue:
                row_issues.append(payment_issue)
            if row_issues:
                issues.extend(row_issues)
                continue

            seen_invoice_ids.add(invoice_id)
            records.append(InvoiceRecord(invoice_id, customer, issued_on, invoice_total, payment_total))

    return ParseResult(tuple(records), tuple(issues))

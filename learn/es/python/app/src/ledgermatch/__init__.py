"""LedgerMatch: conciliación educativa de facturas y pagos."""

from .analytics import CustomerSummary, select_lines, summarize_by_customer
from .parser import CsvSchemaError, ParseResult, read_invoices
from .reconciler import ReconciliationSummary, reconcile
from .reporting import format_report

__all__ = [
    "CsvSchemaError",
    "CustomerSummary",
    "ParseResult",
    "ReconciliationSummary",
    "format_report",
    "read_invoices",
    "reconcile",
    "select_lines",
    "summarize_by_customer",
]

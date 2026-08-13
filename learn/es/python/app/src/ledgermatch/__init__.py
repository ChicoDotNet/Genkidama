"""LedgerMatch: conciliación educativa de facturas y pagos."""

from .analytics import CustomerSummary, select_lines, summarize_by_customer
from .config import ConfigurationError, Settings, load_settings
from .parser import CsvSchemaError, ParseResult, read_invoices
from .reconciler import ReconciliationSummary, reconcile
from .reporting import format_report, write_csv_report, write_json_report
from .service import ImportOutcome, import_csv, sha256_file
from .storage import SaveResult, SqliteRunRepository, StoredRun

__all__ = [
    "ConfigurationError",
    "CsvSchemaError",
    "CustomerSummary",
    "ImportOutcome",
    "ParseResult",
    "ReconciliationSummary",
    "SaveResult",
    "Settings",
    "SqliteRunRepository",
    "StoredRun",
    "format_report",
    "import_csv",
    "load_settings",
    "read_invoices",
    "reconcile",
    "select_lines",
    "sha256_file",
    "summarize_by_customer",
    "write_csv_report",
    "write_json_report",
]

"""LedgerMatch: conciliación educativa de facturas y pagos."""

from .analytics import CustomerSummary, select_lines, summarize_by_customer
from .config import ConfigurationError, Settings, load_settings
from .input_policy import DEFAULT_MAX_INPUT_BYTES, InputPolicyError, OutputPolicyError, validate_input_file, validate_report_destinations
from .parser import CsvSchemaError, ParseResult, read_invoices
from .ports import RunRepository
from .reconciler import ReconciliationSummary, reconcile
from .reporting import format_report, write_csv_report, write_json_report
from .service import ImportOutcome, import_csv, sha256_file
from .storage import SaveResult, SqliteRunRepository, StoredRun

__all__ = [
    "ConfigurationError", "CsvSchemaError", "CustomerSummary", "DEFAULT_MAX_INPUT_BYTES",
    "ImportOutcome", "InputPolicyError", "OutputPolicyError", "ParseResult",
    "ReconciliationSummary", "RunRepository", "SaveResult", "Settings",
    "SqliteRunRepository", "StoredRun", "format_report", "import_csv", "load_settings",
    "read_invoices", "reconcile", "select_lines", "sha256_file", "summarize_by_customer",
    "validate_input_file", "validate_report_destinations", "write_csv_report", "write_json_report",
]

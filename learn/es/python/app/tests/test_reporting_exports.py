import csv
import json
from datetime import date
from decimal import Decimal

from ledgermatch.models import InvoiceRecord, MatchStatus
from ledgermatch.reconciler import ReconciliationLine, ReconciliationSummary
from ledgermatch.reporting import write_csv_report, write_json_report


def _summary():
    record = InvoiceRecord(
        "F-1", "Cliente Uno", date(2026, 8, 1), Decimal("10.00"), Decimal("9.50")
    )
    line = ReconciliationLine(record, MatchStatus.DIFFERENCE, Decimal("-0.50"))
    return ReconciliationSummary((line,), 0)


def test_json_report_preserves_decimal_text(tmp_path):
    path = tmp_path / "report.json"
    write_json_report(path, _summary())

    payload = json.loads(path.read_text(encoding="utf-8"))

    assert payload["summary"]["invoice_total"] == "10.00"
    assert payload["lines"][0]["difference"] == "-0.50"


def test_csv_report_writes_selected_lines(tmp_path):
    path = tmp_path / "report.csv"
    summary = _summary()
    write_csv_report(path, summary.lines)

    with path.open(encoding="utf-8", newline="") as stream:
        rows = list(csv.DictReader(stream))

    assert len(rows) == 1
    assert rows[0]["invoice_id"] == "F-1"
    assert rows[0]["status"] == "difference"

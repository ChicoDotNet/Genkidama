import csv
import json
from datetime import date
from decimal import Decimal
import pytest

from ledgermatch.models import InvoiceRecord, MatchStatus
from ledgermatch.reconciler import ReconciliationLine, ReconciliationSummary
from ledgermatch.reporting import write_csv_report, write_json_report


def _summary(invoice_id="F-1", customer="Cliente Uno"):
    record = InvoiceRecord(invoice_id, customer, date(2026, 8, 1), Decimal("10.00"), Decimal("9.50"))
    return ReconciliationSummary((ReconciliationLine(record, MatchStatus.DIFFERENCE, Decimal("-0.50")),), 0)


def test_json_report_preserves_decimal_text(tmp_path):
    path = tmp_path / "report.json"; write_json_report(path, _summary())
    payload = json.loads(path.read_text(encoding="utf-8"))
    assert payload["summary"]["invoice_total"] == "10.00"
    assert payload["lines"][0]["difference"] == "-0.50"


def test_report_does_not_replace_existing_file_without_force(tmp_path):
    path = tmp_path / "report.json"; path.write_text("existing", encoding="utf-8")
    with pytest.raises(FileExistsError): write_json_report(path, _summary())
    assert path.read_text(encoding="utf-8") == "existing"
    write_json_report(path, _summary(), force=True)
    assert json.loads(path.read_text(encoding="utf-8"))["summary"]["processed"] == 1


def test_csv_report_neutralizes_formula_like_untrusted_text(tmp_path):
    path = tmp_path / "report.csv"; summary = _summary(invoice_id="=1+1", customer="@attacker")
    write_csv_report(path, summary.lines)
    with path.open(encoding="utf-8", newline="") as stream: row = list(csv.DictReader(stream))[0]
    assert row["invoice_id"] == "'=1+1"
    assert row["customer"] == "'@attacker"
    assert row["difference"] == "-0.50"

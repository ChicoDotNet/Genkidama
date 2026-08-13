from datetime import datetime, timezone
from decimal import Decimal

from ledgermatch import SqliteRunRepository
from ledgermatch.models import InvoiceRecord, MatchStatus
from ledgermatch.reconciler import ReconciliationLine, ReconciliationSummary


def _summary():
    record = InvoiceRecord("F-1", "Cliente Uno", datetime(2026, 8, 1, tzinfo=timezone.utc).date(), Decimal("100.00"), Decimal("95.00"))
    return ReconciliationSummary((ReconciliationLine(record, MatchStatus.DIFFERENCE, Decimal("-5.00")),), invalid_rows=1)


def _save(repository, source_name, fingerprint):
    return repository.save_run(source_name=source_name, source_sha256=fingerprint, imported_at=datetime(2026, 8, 12, tzinfo=timezone.utc), summary=_summary())


def test_save_run_persists_and_reopens(tmp_path):
    database = tmp_path / "ledger.db"; repository = SqliteRunRepository(database); result = _save(repository, "invoices.csv", "abc")
    reopened = SqliteRunRepository(database).list_runs()
    assert result.created is True and result.run_id == 1
    assert len(reopened) == 1 and reopened[0].invoice_total == Decimal("100.00") and reopened[0].payment_total == Decimal("95.00")


def test_same_fingerprint_is_idempotent(tmp_path):
    repository = SqliteRunRepository(tmp_path / "ledger.db"); first = _save(repository, "invoices.csv", "same-content"); second = _save(repository, "invoices.csv", "same-content")
    assert first.created is True and second.created is False and second.run_id == first.run_id
    assert len(repository.list_runs()) == 1


def test_list_runs_can_filter_by_exact_source_name(tmp_path):
    repository = SqliteRunRepository(tmp_path / "ledger.db")
    _save(repository, "enero.csv", "enero"); _save(repository, "febrero.csv", "febrero")
    assert tuple(run.source_name for run in repository.list_runs()) == ("febrero.csv", "enero.csv")
    assert tuple(run.source_name for run in repository.list_runs("enero.csv")) == ("enero.csv",)
    assert repository.list_runs("ENERO.csv") == ()

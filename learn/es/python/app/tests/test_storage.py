from datetime import datetime, timezone
from decimal import Decimal

from ledgermatch import SqliteRunRepository
from ledgermatch.models import InvoiceRecord, MatchStatus
from ledgermatch.reconciler import ReconciliationLine, ReconciliationSummary


def _summary():
    record = InvoiceRecord(
        "F-1",
        "Cliente Uno",
        datetime(2026, 8, 1, tzinfo=timezone.utc).date(),
        Decimal("100.00"),
        Decimal("95.00"),
    )
    return ReconciliationSummary(
        (ReconciliationLine(record, MatchStatus.DIFFERENCE, Decimal("-5.00")),),
        invalid_rows=1,
    )


def test_save_run_persists_and_reopens(tmp_path):
    database = tmp_path / "ledger.db"
    repository = SqliteRunRepository(database)

    result = repository.save_run(
        source_name="invoices.csv",
        source_sha256="abc",
        imported_at=datetime(2026, 8, 12, tzinfo=timezone.utc),
        summary=_summary(),
    )

    reopened = SqliteRunRepository(database).list_runs()
    assert result.created is True
    assert result.run_id == 1
    assert len(reopened) == 1
    assert reopened[0].source_name == "invoices.csv"
    assert reopened[0].invoice_total == Decimal("100.00")
    assert reopened[0].payment_total == Decimal("95.00")


def test_same_fingerprint_is_idempotent(tmp_path):
    repository = SqliteRunRepository(tmp_path / "ledger.db")
    kwargs = dict(
        source_name="invoices.csv",
        source_sha256="same-content",
        imported_at=datetime(2026, 8, 12, tzinfo=timezone.utc),
        summary=_summary(),
    )

    first = repository.save_run(**kwargs)
    second = repository.save_run(**kwargs)

    assert first.created is True
    assert second.created is False
    assert second.run_id == first.run_id
    assert len(repository.list_runs()) == 1

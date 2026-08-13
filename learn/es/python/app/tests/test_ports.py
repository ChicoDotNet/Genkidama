from dataclasses import dataclass
from datetime import datetime

from ledgermatch import SaveResult, import_csv
from ledgermatch.reconciler import ReconciliationSummary


@dataclass
class MemoryRunRepository:
    calls: int = 0
    last_summary: ReconciliationSummary | None = None

    def save_run(self, *, source_name: str, source_sha256: str, imported_at: datetime, summary: ReconciliationSummary) -> SaveResult:
        self.calls += 1; self.last_summary = summary
        return SaveResult(run_id=42, created=True)


def test_import_service_accepts_structurally_compatible_repository(tmp_path):
    path = tmp_path / "invoices.csv"
    path.write_text("invoice_id,customer,issued_on,invoice_total,payment_total\nF-1,Acme,2026-08-01,10.00,10.00\n", encoding="utf-8")
    repository = MemoryRunRepository()
    outcome = import_csv(path, repository)
    assert repository.calls == 1
    assert repository.last_summary is outcome.summary
    assert outcome.save == SaveResult(run_id=42, created=True)

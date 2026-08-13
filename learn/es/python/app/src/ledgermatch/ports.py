"""Structural contracts used by LedgerMatch application services."""

from __future__ import annotations

from datetime import datetime
from typing import Protocol

from .reconciler import ReconciliationSummary
from .storage import SaveResult


class RunRepository(Protocol):
    """Describe the persistence capability required by the import service."""

    def save_run(
        self,
        *,
        source_name: str,
        source_sha256: str,
        imported_at: datetime,
        summary: ReconciliationSummary,
    ) -> SaveResult:
        """Persist or reuse one reconciliation run and return its identity."""
        ...

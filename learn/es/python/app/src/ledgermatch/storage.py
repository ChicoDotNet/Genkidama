"""Persist LedgerMatch reconciliation runs in SQLite."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime
from decimal import Decimal
from pathlib import Path
import sqlite3

from .reconciler import ReconciliationSummary


@dataclass(frozen=True, slots=True)
class SaveResult:
    """Describe the persistence outcome for one reconciliation import."""
    run_id: int
    created: bool


@dataclass(frozen=True, slots=True)
class StoredRun:
    """Describe one persisted reconciliation import without loading its detail."""
    run_id: int
    source_name: str
    source_sha256: str
    imported_at: datetime
    line_count: int
    invalid_rows: int
    invoice_total: Decimal
    payment_total: Decimal


class SqliteRunRepository:
    """Store reconciliation imports atomically in a local SQLite database."""

    def __init__(self, database: str | Path) -> None:
        """Create a repository targeting one SQLite database path."""
        self._database = Path(database)

    def save_run(self, *, source_name: str, source_sha256: str, imported_at: datetime, summary: ReconciliationSummary) -> SaveResult:
        """Persist one run and accepted lines atomically; reuse an existing fingerprint."""
        self._database.parent.mkdir(parents=True, exist_ok=True)
        connection = sqlite3.connect(self._database, autocommit=False)
        try:
            connection.execute("PRAGMA foreign_keys = ON")
            self._ensure_schema(connection)
            cursor = connection.execute(
                """
                INSERT INTO reconciliation_runs(source_name, source_sha256, imported_at, line_count, invalid_rows, invoice_total, payment_total)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(source_sha256) DO NOTHING
                """,
                (source_name, source_sha256, imported_at.isoformat(), len(summary.lines), summary.invalid_rows, str(summary.invoice_total), str(summary.payment_total)),
            )
            if cursor.rowcount == 0:
                row = connection.execute("SELECT id FROM reconciliation_runs WHERE source_sha256 = ?", (source_sha256,)).fetchone()
                if row is None:
                    raise RuntimeError("No se pudo recuperar la importación existente.")
                connection.rollback()
                return SaveResult(run_id=int(row[0]), created=False)
            run_id = int(cursor.lastrowid)
            connection.executemany(
                """
                INSERT INTO reconciliation_lines(run_id, invoice_id, customer, issued_on, invoice_total, payment_total, status, difference)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                ((run_id, line.record.invoice_id, line.record.customer, line.record.issued_on.isoformat(), str(line.record.invoice_total), str(line.record.payment_total), line.status.value, str(line.difference)) for line in summary.lines),
            )
            connection.commit()
            return SaveResult(run_id=run_id, created=True)
        except Exception:
            connection.rollback()
            raise
        finally:
            connection.close()

    def list_runs(self, source_name: str | None = None) -> tuple[StoredRun, ...]:
        """Return runs newest-first, optionally filtering by exact source filename."""
        if not self._database.exists():
            return ()
        connection = sqlite3.connect(self._database)
        try:
            self._ensure_schema(connection)
            if source_name is None:
                rows = connection.execute(
                    "SELECT id, source_name, source_sha256, imported_at, line_count, invalid_rows, invoice_total, payment_total FROM reconciliation_runs ORDER BY id DESC"
                ).fetchall()
            else:
                rows = connection.execute(
                    "SELECT id, source_name, source_sha256, imported_at, line_count, invalid_rows, invoice_total, payment_total FROM reconciliation_runs WHERE source_name = ? ORDER BY id DESC",
                    (source_name,),
                ).fetchall()
            return tuple(
                StoredRun(int(row[0]), str(row[1]), str(row[2]), datetime.fromisoformat(str(row[3])), int(row[4]), int(row[5]), Decimal(str(row[6])), Decimal(str(row[7])))
                for row in rows
            )
        finally:
            connection.close()

    @staticmethod
    def _ensure_schema(connection: sqlite3.Connection) -> None:
        connection.executescript(
            """
            CREATE TABLE IF NOT EXISTS reconciliation_runs(
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                source_name TEXT NOT NULL,
                source_sha256 TEXT NOT NULL UNIQUE,
                imported_at TEXT NOT NULL,
                line_count INTEGER NOT NULL,
                invalid_rows INTEGER NOT NULL,
                invoice_total TEXT NOT NULL,
                payment_total TEXT NOT NULL
            );
            CREATE TABLE IF NOT EXISTS reconciliation_lines(
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                invoice_id TEXT NOT NULL,
                customer TEXT NOT NULL,
                issued_on TEXT NOT NULL,
                invoice_total TEXT NOT NULL,
                payment_total TEXT NOT NULL,
                status TEXT NOT NULL,
                difference TEXT NOT NULL,
                FOREIGN KEY(run_id) REFERENCES reconciliation_runs(id) ON DELETE CASCADE
            );
            """
        )

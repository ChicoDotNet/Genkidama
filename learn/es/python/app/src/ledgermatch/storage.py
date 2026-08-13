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
    """Describe the persistence outcome for one reconciliation import.

    Attributes:
        run_id: Stable SQLite identifier of the stored or reused run.
        created: ``True`` only when this call created a new run.
    """

    run_id: int
    created: bool


@dataclass(frozen=True, slots=True)
class StoredRun:
    """Describe one persisted reconciliation import without loading its detail.

    Attributes:
        run_id: SQLite identifier.
        source_name: Original input filename without directory components.
        source_sha256: SHA-256 fingerprint used for idempotency.
        imported_at: Timestamp recorded when the run was first stored.
        line_count: Number of accepted reconciliation lines.
        invalid_rows: Number of input rows rejected by validation.
        invoice_total: Exact decimal total invoiced.
        payment_total: Exact decimal total paid.
    """

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
        """Create a repository targeting one SQLite database path.

        Args:
            database: File path for SQLite. The file is opened lazily.

        Side Effects:
            No file is created until an operation needs the database.
        """
        self._database = Path(database)

    def save_run(
        self,
        *,
        source_name: str,
        source_sha256: str,
        imported_at: datetime,
        summary: ReconciliationSummary,
    ) -> SaveResult:
        """Persist one run and all accepted lines as one transaction.

        Args:
            source_name: Display filename of the imported source.
            source_sha256: Hexadecimal fingerprint identifying source content.
            imported_at: Timestamp to persist for a newly created run.
            summary: Reconciliation summary and accepted lines to store.

        Returns:
            A stable run identifier and whether a new run was created. Repeating
            the same SHA-256 fingerprint is idempotent and reuses the old id.

        Raises:
            sqlite3.Error: SQLite cannot initialize or persist the transaction.
            OSError: The database parent directory cannot be created.

        Side Effects:
            Creates the database directory/file as needed and commits all new
            run data atomically. On failure, pending changes are rolled back.
        """
        self._database.parent.mkdir(parents=True, exist_ok=True)
        connection = sqlite3.connect(self._database, autocommit=False)
        try:
            connection.execute("PRAGMA foreign_keys = ON")
            self._ensure_schema(connection)
            cursor = connection.execute(
                """
                INSERT INTO reconciliation_runs(
                    source_name, source_sha256, imported_at, line_count,
                    invalid_rows, invoice_total, payment_total
                )
                VALUES (?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(source_sha256) DO NOTHING
                """,
                (
                    source_name,
                    source_sha256,
                    imported_at.isoformat(),
                    len(summary.lines),
                    summary.invalid_rows,
                    str(summary.invoice_total),
                    str(summary.payment_total),
                ),
            )

            if cursor.rowcount == 0:
                row = connection.execute(
                    "SELECT id FROM reconciliation_runs WHERE source_sha256 = ?",
                    (source_sha256,),
                ).fetchone()
                if row is None:
                    raise RuntimeError("No se pudo recuperar la importación existente.")
                connection.rollback()
                return SaveResult(run_id=int(row[0]), created=False)

            run_id = int(cursor.lastrowid)
            connection.executemany(
                """
                INSERT INTO reconciliation_lines(
                    run_id, invoice_id, customer, issued_on, invoice_total,
                    payment_total, status, difference
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    (
                        run_id,
                        line.record.invoice_id,
                        line.record.customer,
                        line.record.issued_on.isoformat(),
                        str(line.record.invoice_total),
                        str(line.record.payment_total),
                        line.status.value,
                        str(line.difference),
                    )
                    for line in summary.lines
                ),
            )
            connection.commit()
            return SaveResult(run_id=run_id, created=True)
        except Exception:
            connection.rollback()
            raise
        finally:
            connection.close()

    def list_runs(self) -> tuple[StoredRun, ...]:
        """Return persisted imports from newest identifier to oldest.

        Returns:
            Immutable run summaries. An unopened/nonexistent database produces
            an empty tuple rather than creating a file.

        Raises:
            sqlite3.Error: An existing database cannot be initialized or read.

        Side Effects:
            May create missing schema tables inside an existing database file.
        """
        if not self._database.exists():
            return ()

        connection = sqlite3.connect(self._database)
        try:
            self._ensure_schema(connection)
            rows = connection.execute(
                """
                SELECT id, source_name, source_sha256, imported_at, line_count,
                       invalid_rows, invoice_total, payment_total
                FROM reconciliation_runs
                ORDER BY id DESC
                """
            ).fetchall()
            return tuple(
                StoredRun(
                    run_id=int(row[0]),
                    source_name=str(row[1]),
                    source_sha256=str(row[2]),
                    imported_at=datetime.fromisoformat(str(row[3])),
                    line_count=int(row[4]),
                    invalid_rows=int(row[5]),
                    invoice_total=Decimal(str(row[6])),
                    payment_total=Decimal(str(row[7])),
                )
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

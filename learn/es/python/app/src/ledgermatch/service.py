"""Coordinate LedgerMatch parsing, reconciliation, fingerprinting and persistence."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
from pathlib import Path

from .parser import ParseResult, read_invoices
from .reconciler import ReconciliationSummary, reconcile
from .storage import SaveResult, SqliteRunRepository


@dataclass(frozen=True, slots=True)
class ImportOutcome:
    """Contain the observable result of one CSV import.

    Attributes:
        parsed: Accepted rows and validation issues from the input boundary.
        summary: Deterministic reconciliation of accepted rows.
        save: Persistence result, including whether the content was new.
        source_sha256: Hexadecimal SHA-256 fingerprint of the source bytes.
    """

    parsed: ParseResult
    summary: ReconciliationSummary
    save: SaveResult
    source_sha256: str


def sha256_file(path: str | Path) -> str:
    """Calculate a deterministic SHA-256 fingerprint without loading the whole file.

    Args:
        path: File whose raw bytes will be fingerprinted.

    Returns:
        Lowercase hexadecimal SHA-256 digest.

    Raises:
        FileNotFoundError: The source file does not exist.
        OSError: The file cannot be read.

    Side Effects:
        Reads the file in 64 KiB chunks and does not modify it.
    """
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(64 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def import_csv(
    path: str | Path,
    repository: SqliteRunRepository,
    *,
    imported_at: datetime | None = None,
) -> ImportOutcome:
    """Parse, reconcile, fingerprint and persist one CSV input.

    Args:
        path: Source CSV path.
        repository: Repository receiving the reconciliation result.
        imported_at: Optional timestamp for deterministic tests. UTC ``now`` is
            used when omitted.

    Returns:
        Parsed data, reconciliation summary, persistence result and fingerprint.

    Raises:
        FileNotFoundError: The source file does not exist.
        CsvSchemaError: Propagated by ``read_invoices`` for an invalid schema.
        sqlite3.Error: Persistence fails.

    Side Effects:
        Reads the source file and writes through ``repository``.
    """
    source = Path(path)
    parsed = read_invoices(source)
    summary = reconcile(parsed)
    fingerprint = sha256_file(source)
    save = repository.save_run(
        source_name=source.name,
        source_sha256=fingerprint,
        imported_at=imported_at or datetime.now(timezone.utc),
        summary=summary,
    )
    return ImportOutcome(parsed=parsed, summary=summary, save=save, source_sha256=fingerprint)

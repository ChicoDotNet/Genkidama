"""Coordinate LedgerMatch parsing, reconciliation, fingerprinting and persistence."""

from __future__ import annotations

from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
from pathlib import Path

from .input_policy import DEFAULT_MAX_INPUT_BYTES, validate_input_file
from .parser import ParseResult, read_invoices
from .ports import RunRepository
from .reconciler import ReconciliationSummary, reconcile
from .storage import SaveResult


@dataclass(frozen=True, slots=True)
class ImportOutcome:
    """Contain the observable result of one CSV import."""

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
    repository: RunRepository,
    *,
    imported_at: datetime | None = None,
    max_input_bytes: int = DEFAULT_MAX_INPUT_BYTES,
) -> ImportOutcome:
    """Validate, parse, reconcile, fingerprint and persist one CSV input.

    Args:
        path: Source CSV path.
        repository: Object satisfying the ``RunRepository`` protocol.
        imported_at: Optional timestamp for deterministic tests.
        max_input_bytes: Positive maximum source size accepted before reading.

    Returns:
        Parsed data, reconciliation summary, persistence result and fingerprint.

    Raises:
        InputPolicyError: The source is not a regular file or exceeds the limit.
        FileNotFoundError: The source cannot be found during subsequent I/O.
        CsvSchemaError: The CSV schema is invalid.
        OSError: The source cannot be read.
        Exception: Repository-specific persistence errors propagate.

    Side Effects:
        Reads the source file and writes through ``repository`` after validation.
    """
    source = validate_input_file(path, max_bytes=max_input_bytes)
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

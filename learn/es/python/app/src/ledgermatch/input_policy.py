"""Validate local input and output paths before LedgerMatch performs I/O."""

from __future__ import annotations

from collections.abc import Iterable
from pathlib import Path

DEFAULT_MAX_INPUT_BYTES = 10 * 1024 * 1024


class InputPolicyError(ValueError):
    """Raised when an input violates LedgerMatch file-boundary policy."""


class OutputPolicyError(ValueError):
    """Raised when report destinations could overwrite important inputs or each other."""


def validate_input_file(
    path: str | Path,
    *,
    max_bytes: int = DEFAULT_MAX_INPUT_BYTES,
) -> Path:
    """Validate that a source is a regular file within the configured size limit.

    Args:
        path: Candidate input path.
        max_bytes: Maximum accepted byte length. Must be positive.

    Returns:
        The ``Path`` object after successful validation.

    Raises:
        InputPolicyError: ``max_bytes`` is invalid, the path is not a regular
            file, or the file exceeds the configured limit.
        OSError: Metadata for an existing path cannot be read.

    Side Effects:
        Reads filesystem metadata only; file contents are not opened.
    """
    if max_bytes <= 0:
        raise InputPolicyError("El límite de entrada debe ser mayor que cero.")

    source = Path(path)
    if not source.is_file():
        raise InputPolicyError(f"La entrada no es un archivo regular: {source}")

    size = source.stat().st_size
    if size > max_bytes:
        raise InputPolicyError(
            f"El archivo excede el límite de {max_bytes} bytes: {size} bytes."
        )
    return source


def validate_report_destinations(
    source: str | Path,
    destinations: Iterable[str | Path],
) -> None:
    """Reject report destinations that collide with the source or one another.

    Args:
        source: Input CSV path that must never be replaced by an export.
        destinations: Candidate JSON/CSV report paths. Nonexistent paths are
            allowed.

    Raises:
        OutputPolicyError: A destination resolves to the source path or two
            report destinations resolve to the same path.

    Side Effects:
        Resolves paths but creates no files.
    """
    source_path = Path(source).resolve()
    seen: set[Path] = set()

    for raw_destination in destinations:
        destination = Path(raw_destination).resolve()
        if destination == source_path:
            raise OutputPolicyError(
                "Un reporte no puede usar la misma ruta que el CSV de entrada."
            )
        if destination in seen:
            raise OutputPolicyError(
                "Los reportes JSON y CSV no pueden usar la misma ruta."
            )
        seen.add(destination)

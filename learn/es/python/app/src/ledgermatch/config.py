"""Load LedgerMatch runtime configuration from explicit values and environment variables."""

from __future__ import annotations

from dataclasses import dataclass
import os
from pathlib import Path

from .input_policy import DEFAULT_MAX_INPUT_BYTES

_DEFAULT_DB = "ledgermatch.db"


class ConfigurationError(ValueError):
    """Raised when LedgerMatch runtime configuration is invalid."""


@dataclass(frozen=True, slots=True)
class Settings:
    """Contain validated runtime settings used by the command-line application."""

    database: Path
    max_input_bytes: int


def load_settings(
    *,
    database: str | Path | None = None,
    max_input_bytes: int | None = None,
) -> Settings:
    """Load LedgerMatch settings with explicit values taking precedence.

    Args:
        database: Optional SQLite path. Otherwise ``LEDGERMATCH_DB`` or the
            local default ``ledgermatch.db`` is used.
        max_input_bytes: Optional positive input limit. Otherwise
            ``LEDGERMATCH_MAX_INPUT_BYTES`` or 10 MiB is used.

    Returns:
        Validated immutable settings.

    Raises:
        ConfigurationError: A path is blank or the size limit is not a positive
            integer.

    Side Effects:
        Reads environment variables only for values not supplied explicitly.
    """
    raw_database = database if database is not None else os.getenv("LEDGERMATCH_DB", _DEFAULT_DB)
    if not str(raw_database).strip():
        raise ConfigurationError("La ruta de base de datos no puede estar vacía.")

    raw_limit: object = max_input_bytes
    if raw_limit is None:
        raw_limit = os.getenv("LEDGERMATCH_MAX_INPUT_BYTES", str(DEFAULT_MAX_INPUT_BYTES))
    try:
        parsed_limit = int(raw_limit)
    except (TypeError, ValueError) as exc:
        raise ConfigurationError("El límite de entrada debe ser un entero positivo.") from exc
    if parsed_limit <= 0:
        raise ConfigurationError("El límite de entrada debe ser un entero positivo.")

    return Settings(database=Path(raw_database).expanduser(), max_input_bytes=parsed_limit)

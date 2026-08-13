"""Load LedgerMatch runtime configuration from explicit values and environment variables."""

from __future__ import annotations

from dataclasses import dataclass
import os
from pathlib import Path

_DEFAULT_DB = "ledgermatch.db"


class ConfigurationError(ValueError):
    """Raised when LedgerMatch runtime configuration is invalid."""


@dataclass(frozen=True, slots=True)
class Settings:
    """Contain validated runtime settings used by the command-line application.

    Attributes:
        database: SQLite database path. The path is not opened by this type.
    """

    database: Path


def load_settings(*, database: str | Path | None = None) -> Settings:
    """Load LedgerMatch settings with explicit values taking precedence.

    Args:
        database: Optional SQLite path supplied by the caller. When omitted,
            ``LEDGERMATCH_DB`` is read and finally ``ledgermatch.db`` is used.

    Returns:
        Validated immutable settings. ``~`` is expanded in the resulting path.

    Raises:
        ConfigurationError: The selected database path is blank.

    Side Effects:
        Reads ``LEDGERMATCH_DB`` only when ``database`` is not supplied.
    """
    raw = database if database is not None else os.getenv("LEDGERMATCH_DB", _DEFAULT_DB)
    if not str(raw).strip():
        raise ConfigurationError("La ruta de base de datos no puede estar vacía.")
    return Settings(database=Path(raw).expanduser())

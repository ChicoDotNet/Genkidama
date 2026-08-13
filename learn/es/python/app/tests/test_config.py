from pathlib import Path

import pytest

from ledgermatch.config import ConfigurationError, load_settings


def test_environment_database_is_used_when_no_explicit_value(monkeypatch):
    monkeypatch.setenv("LEDGERMATCH_DB", "from-env.db")
    assert load_settings().database == Path("from-env.db")


def test_explicit_database_wins_over_environment(monkeypatch):
    monkeypatch.setenv("LEDGERMATCH_DB", "from-env.db")
    assert load_settings(database="explicit.db").database == Path("explicit.db")


def test_empty_environment_database_is_rejected(monkeypatch):
    monkeypatch.setenv("LEDGERMATCH_DB", "")
    with pytest.raises(ConfigurationError):
        load_settings()

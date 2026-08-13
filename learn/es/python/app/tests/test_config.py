from pathlib import Path

import pytest

from ledgermatch.config import ConfigurationError, load_settings
from ledgermatch.input_policy import DEFAULT_MAX_INPUT_BYTES


def test_environment_database_is_used_when_no_explicit_value(monkeypatch):
    monkeypatch.setenv("LEDGERMATCH_DB", "from-env.db")
    assert load_settings().database == Path("from-env.db")


def test_explicit_database_wins_over_environment(monkeypatch):
    monkeypatch.setenv("LEDGERMATCH_DB", "from-env.db")
    assert load_settings(database="explicit.db").database == Path("explicit.db")


def test_default_input_limit_is_ten_mib(monkeypatch):
    monkeypatch.delenv("LEDGERMATCH_MAX_INPUT_BYTES", raising=False)
    assert load_settings().max_input_bytes == DEFAULT_MAX_INPUT_BYTES


def test_explicit_input_limit_wins_over_environment(monkeypatch):
    monkeypatch.setenv("LEDGERMATCH_MAX_INPUT_BYTES", "100")
    assert load_settings(max_input_bytes=200).max_input_bytes == 200


@pytest.mark.parametrize("raw", ["", "abc", "0", "-1"])
def test_invalid_environment_input_limit_is_rejected(monkeypatch, raw):
    monkeypatch.setenv("LEDGERMATCH_MAX_INPUT_BYTES", raw)
    with pytest.raises(ConfigurationError, match="entero positivo"):
        load_settings()

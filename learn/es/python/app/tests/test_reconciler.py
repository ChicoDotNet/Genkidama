from decimal import Decimal

import pytest

from ledgermatch import CsvSchemaError, read_invoices, reconcile


def _write(tmp_path, text: str):
    path = tmp_path / "invoices.csv"
    path.write_text(text, encoding="utf-8")
    return path


def test_reconcile_counts_matches_and_differences(tmp_path):
    path = _write(
        tmp_path,
        "invoice_id,customer,issued_on,invoice_total,payment_total\n"
        "F-1,Cliente Uno,2026-08-01,100.10,100.10\n"
        "F-2,Cliente Dos,2026-08-02,50.00,49.50\n",
    )

    summary = reconcile(read_invoices(path))

    assert summary.matched == 1
    assert summary.different == 1
    assert summary.invoice_total == Decimal("150.10")
    assert summary.payment_total == Decimal("149.60")
    assert summary.lines[1].difference == Decimal("-0.50")


def test_invalid_money_becomes_a_validation_issue(tmp_path):
    path = _write(
        tmp_path,
        "invoice_id,customer,issued_on,invoice_total,payment_total\n"
        "F-1,Cliente Uno,2026-08-01,no-es-dinero,100.00\n",
    )

    parsed = read_invoices(path)

    assert parsed.records == ()
    assert len(parsed.issues) == 1
    assert parsed.issues[0].field == "invoice_total"


def test_missing_required_columns_fails_fast(tmp_path):
    path = _write(tmp_path, "invoice_id,customer\nF-1,Cliente Uno\n")

    with pytest.raises(CsvSchemaError, match="Faltan columnas obligatorias"):
        read_invoices(path)

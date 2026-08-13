import sys

from ledgermatch.__main__ import main


def test_cli_filters_detail_by_customer_and_difference(tmp_path, monkeypatch, capsys):
    path = tmp_path / "invoices.csv"
    path.write_text(
        "invoice_id,customer,issued_on,invoice_total,payment_total\n"
        "F-1,Cliente Uno,2026-08-01,100.00,100.00\n"
        "F-2,Cliente Uno,2026-08-02,50.00,45.00\n"
        "F-3,Cliente Dos,2026-08-03,20.00,25.00\n",
        encoding="utf-8",
    )
    monkeypatch.setattr(
        sys,
        "argv",
        ["ledgermatch", str(path), "--customer", "cliente uno", "--only-differences"],
    )

    exit_code = main()
    output = capsys.readouterr().out

    assert exit_code == 0
    assert "Detalle seleccionado: 1" in output
    assert "F-2 | Cliente Uno | difference" in output
    assert "F-1 | Cliente Uno" not in output
    assert "F-3 | Cliente Dos" not in output

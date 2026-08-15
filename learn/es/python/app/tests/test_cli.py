import sys

from ledgermatch.__main__ import main


def _source(tmp_path):
    path = tmp_path / "invoices.csv"
    path.write_text(
        "invoice_id,customer,issued_on,invoice_total,payment_total\n"
        "F-1,Cliente Uno,2026-08-01,100.00,100.00\n"
        "F-2,Cliente Uno,2026-08-02,50.00,45.00\n"
        "F-3,Cliente Dos,2026-08-03,20.00,25.00\n",
        encoding="utf-8",
    )
    return path


def test_cli_filters_detail_by_customer_and_difference(tmp_path, monkeypatch, capsys):
    path = _source(tmp_path)
    monkeypatch.setattr(sys, "argv", ["ledgermatch", str(path), "--db", str(tmp_path / "ledger.db"), "--customer", "cliente uno", "--only-differences"])
    assert main() == 0
    output = capsys.readouterr().out
    assert "Detalle seleccionado: 1" in output
    assert "F-2 | Cliente Uno | difference" in output
    assert "F-1 | Cliente Uno" not in output
    assert "F-3 | Cliente Dos" not in output


def test_cli_persists_idempotently_and_exports_with_explicit_overwrite(tmp_path, monkeypatch, capsys):
    path = _source(tmp_path); database = tmp_path / "ledger.db"; report = tmp_path / "report.json"
    base = ["ledgermatch", str(path), "--db", str(database), "--only-differences", "--json", str(report)]
    monkeypatch.setattr(sys, "argv", base); assert main() == 0; first = capsys.readouterr().out
    monkeypatch.setattr(sys, "argv", [*base, "--force"]); assert main() == 0; second = capsys.readouterr().out
    assert "Persistencia: importación #1 creada" in first
    assert "Persistencia: importación #1 ya registrada" in second
    assert report.exists()


def test_cli_refuses_existing_report_without_force(tmp_path, monkeypatch, capsys):
    path = _source(tmp_path); report = tmp_path / "report.json"; report.write_text("do-not-replace", encoding="utf-8")
    monkeypatch.setattr(sys, "argv", ["ledgermatch", str(path), "--db", str(tmp_path / "ledger.db"), "--json", str(report)])
    assert main() == 2
    assert "Usa --force" in capsys.readouterr().out
    assert report.read_text(encoding="utf-8") == "do-not-replace"


def test_cli_rejects_source_over_configured_limit(tmp_path, monkeypatch, capsys):
    path = _source(tmp_path)
    monkeypatch.setattr(sys, "argv", ["ledgermatch", str(path), "--db", str(tmp_path / "ledger.db"), "--max-input-bytes", "10"])
    assert main() == 2
    assert "excede el límite" in capsys.readouterr().out


def test_cli_rejects_report_that_would_replace_source_even_with_force(tmp_path, monkeypatch, capsys):
    path = _source(tmp_path); original = path.read_text(encoding="utf-8")
    monkeypatch.setattr(sys, "argv", ["ledgermatch", str(path), "--db", str(tmp_path / "ledger.db"), "--json", str(path), "--force"])
    assert main() == 2
    assert "misma ruta que el CSV de entrada" in capsys.readouterr().out
    assert path.read_text(encoding="utf-8") == original
    assert not (tmp_path / "ledger.db").exists()


def test_cli_rejects_json_and_csv_using_same_destination(tmp_path, monkeypatch, capsys):
    path = _source(tmp_path); report = tmp_path / "report.out"
    monkeypatch.setattr(sys, "argv", ["ledgermatch", str(path), "--db", str(tmp_path / "ledger.db"), "--json", str(report), "--csv", str(report)])
    assert main() == 2
    assert "no pueden usar la misma ruta" in capsys.readouterr().out
    assert not report.exists()
    assert not (tmp_path / "ledger.db").exists()

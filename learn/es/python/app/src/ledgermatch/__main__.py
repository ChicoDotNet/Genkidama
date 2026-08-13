"""Command-line entry point for the LedgerMatch educational application."""

from __future__ import annotations

import argparse
from pathlib import Path

from .parser import CsvSchemaError, read_invoices
from .reconciler import reconcile


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="ledgermatch",
        description="Concilia importes facturados y pagados desde un CSV.",
    )
    parser.add_argument("csv_file", type=Path, help="Archivo CSV a conciliar")
    return parser


def main() -> int:
    """Run LedgerMatch from command-line arguments and return a process code."""

    args = _parser().parse_args()

    try:
        parsed = read_invoices(args.csv_file)
    except FileNotFoundError:
        print(f"No existe el archivo: {args.csv_file}")
        return 2
    except CsvSchemaError as exc:
        print(f"CSV inválido: {exc}")
        return 2

    summary = reconcile(parsed)
    print("LedgerMatch")
    print(f"Procesadas: {len(summary.lines)}")
    print(f"Coinciden: {summary.matched}")
    print(f"Con diferencia: {summary.different}")
    print(f"Filas inválidas: {summary.invalid_rows}")
    print(f"Total facturado: {summary.invoice_total:.2f}")
    print(f"Total pagado: {summary.payment_total:.2f}")

    for line in summary.lines:
        if line.difference:
            print(f"- {line.record.invoice_id}: diferencia {line.difference:+.2f}")

    for issue in parsed.issues:
        print(f"! fila {issue.row_number} / {issue.field}: {issue.message}")

    return 0 if not parsed.issues else 1


if __name__ == "__main__":
    raise SystemExit(main())

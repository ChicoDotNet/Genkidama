"""Command-line entry point for the LedgerMatch educational application."""

from __future__ import annotations

import argparse
from pathlib import Path

from .analytics import select_lines
from .parser import CsvSchemaError, read_invoices
from .reconciler import reconcile
from .reporting import format_report


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="ledgermatch",
        description="Concilia importes facturados y pagados desde un CSV.",
    )
    parser.add_argument("csv_file", type=Path, help="Archivo CSV a conciliar")
    parser.add_argument("--customer", help="Muestra detalle sólo para este cliente")
    parser.add_argument(
        "--only-differences",
        action="store_true",
        help="Muestra en el detalle sólo registros con diferencia",
    )
    parser.add_argument(
        "--by-customer",
        action="store_true",
        help="Agrega un resumen de totales agrupado por cliente",
    )
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
    selected = select_lines(
        summary,
        customer=args.customer,
        only_differences=args.only_differences,
    )
    print(
        format_report(
            summary,
            selected_lines=selected,
            include_customers=args.by_customer,
        )
    )

    for issue in parsed.issues:
        print(f"! fila {issue.row_number} / {issue.field}: {issue.message}")

    return 0 if not parsed.issues else 1


if __name__ == "__main__":
    raise SystemExit(main())

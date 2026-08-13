"""Command-line entry point for the LedgerMatch educational application."""

from __future__ import annotations

import argparse
import logging
from pathlib import Path

from .analytics import select_lines
from .config import ConfigurationError, load_settings
from .parser import CsvSchemaError
from .reporting import format_report, write_csv_report, write_json_report
from .service import import_csv
from .storage import SqliteRunRepository

logger = logging.getLogger(__name__)


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
        help="Muestra sólo registros con diferencia",
    )
    parser.add_argument(
        "--by-customer",
        action="store_true",
        help="Agrega un resumen agrupado por cliente",
    )
    parser.add_argument("--db", type=Path, help="Ruta SQLite; sobreescribe LEDGERMATCH_DB")
    parser.add_argument("--json", type=Path, dest="json_report", help="Exporta el detalle seleccionado a JSON")
    parser.add_argument("--csv", type=Path, dest="csv_report", help="Exporta el detalle seleccionado a CSV")
    parser.add_argument("--verbose", action="store_true", help="Muestra diagnóstico operacional adicional")
    return parser


def _configure_logging(verbose: bool) -> None:
    logging.basicConfig(
        level=logging.DEBUG if verbose else logging.WARNING,
        format="%(levelname)s %(name)s: %(message)s",
    )


def main() -> int:
    """Run LedgerMatch from command-line arguments and return a process code."""
    args = _parser().parse_args()
    _configure_logging(args.verbose)
    try:
        settings = load_settings(database=args.db)
        repository = SqliteRunRepository(settings.database)
        outcome = import_csv(args.csv_file, repository)
    except FileNotFoundError:
        print(f"No existe el archivo: {args.csv_file}")
        return 2
    except (CsvSchemaError, ConfigurationError) as exc:
        print(f"Entrada inválida: {exc}")
        return 2

    logger.info(
        "Importación %s (run_id=%s, created=%s)",
        args.csv_file.name,
        outcome.save.run_id,
        outcome.save.created,
    )
    selected = select_lines(
        outcome.summary,
        customer=args.customer,
        only_differences=args.only_differences,
    )
    print(format_report(outcome.summary, selected_lines=selected, include_customers=args.by_customer))
    print(
        f"Persistencia: importación #{outcome.save.run_id} "
        + ("creada" if outcome.save.created else "ya registrada")
    )

    if args.json_report is not None:
        write_json_report(args.json_report, outcome.summary, selected_lines=selected)
        print(f"JSON: {args.json_report}")
    if args.csv_report is not None:
        write_csv_report(args.csv_report, selected)
        print(f"CSV: {args.csv_report}")

    for issue in outcome.parsed.issues:
        print(f"! fila {issue.row_number} / {issue.field}: {issue.message}")
    return 0 if not outcome.parsed.issues else 1


if __name__ == "__main__":
    raise SystemExit(main())

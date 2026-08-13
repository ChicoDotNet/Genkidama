"""Command-line entry point for the LedgerMatch educational application."""

from __future__ import annotations

import argparse
import logging
from pathlib import Path
import sqlite3

from .analytics import select_lines
from .config import ConfigurationError, load_settings
from .input_policy import InputPolicyError, OutputPolicyError, validate_report_destinations
from .parser import CsvSchemaError
from .reporting import format_report, write_csv_report, write_json_report
from .service import import_csv
from .storage import SqliteRunRepository

logger = logging.getLogger(__name__)


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="ledgermatch", description="Concilia importes facturados y pagados desde un CSV.")
    parser.add_argument("csv_file", type=Path, help="Archivo CSV a conciliar")
    parser.add_argument("--customer", help="Muestra detalle sólo para este cliente")
    parser.add_argument("--only-differences", action="store_true", help="Muestra sólo registros con diferencia")
    parser.add_argument("--by-customer", action="store_true", help="Agrega un resumen agrupado por cliente")
    parser.add_argument("--db", type=Path, help="Ruta SQLite; sobreescribe LEDGERMATCH_DB")
    parser.add_argument("--max-input-bytes", type=int, help="Límite de entrada; sobreescribe LEDGERMATCH_MAX_INPUT_BYTES")
    parser.add_argument("--json", type=Path, dest="json_report", help="Exporta el detalle seleccionado a JSON")
    parser.add_argument("--csv", type=Path, dest="csv_report", help="Exporta el detalle seleccionado a CSV")
    parser.add_argument("--force", action="store_true", help="Permite reemplazar archivos de reporte existentes")
    parser.add_argument("--verbose", action="store_true", help="Muestra diagnóstico operacional adicional")
    return parser


def _configure_logging(verbose: bool) -> None:
    logging.basicConfig(level=logging.DEBUG if verbose else logging.WARNING, format="%(levelname)s %(name)s: %(message)s", force=True)


def main() -> int:
    """Run LedgerMatch and return 0 success, 1 row issues, or 2 boundary failure."""
    args = _parser().parse_args()
    _configure_logging(args.verbose)
    try:
        settings = load_settings(database=args.db, max_input_bytes=args.max_input_bytes)
        destinations = tuple(path for path in (args.json_report, args.csv_report) if path is not None)
        validate_report_destinations(args.csv_file, destinations)
        repository = SqliteRunRepository(settings.database)
        outcome = import_csv(args.csv_file, repository, max_input_bytes=settings.max_input_bytes)
    except (CsvSchemaError, ConfigurationError, InputPolicyError, OutputPolicyError, sqlite3.Error, OSError) as exc:
        print(f"Entrada no procesada: {exc}")
        return 2

    logger.info("Importación %s (run_id=%s, created=%s)", args.csv_file.name, outcome.save.run_id, outcome.save.created)
    selected = select_lines(outcome.summary, customer=args.customer, only_differences=args.only_differences)
    print(format_report(outcome.summary, selected_lines=selected, include_customers=args.by_customer))
    print(f"Persistencia: importación #{outcome.save.run_id} " + ("creada" if outcome.save.created else "ya registrada"))
    try:
        if args.json_report is not None:
            write_json_report(args.json_report, outcome.summary, selected_lines=selected, force=args.force)
            print(f"JSON: {args.json_report}")
        if args.csv_report is not None:
            write_csv_report(args.csv_report, selected, force=args.force)
            print(f"CSV: {args.csv_report}")
    except FileExistsError as exc:
        print(f"Reporte no escrito: {exc}. Usa --force para reemplazarlo.")
        return 2
    except OSError as exc:
        print(f"Reporte no escrito: {exc}")
        return 2
    for issue in outcome.parsed.issues:
        print(f"! fila {issue.row_number} / {issue.field}: {issue.message}")
    return 0 if not outcome.parsed.issues else 1


if __name__ == "__main__":
    raise SystemExit(main())

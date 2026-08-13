"""Format and export LedgerMatch reconciliation results."""

from __future__ import annotations

import csv
import json
from pathlib import Path
from typing import TextIO

from .analytics import summarize_by_customer
from .reconciler import ReconciliationLine, ReconciliationSummary

_SPREADSHEET_FORMULA_PREFIXES = ("=", "+", "-", "@", "\t", "\r")


def format_report(
    summary: ReconciliationSummary,
    *,
    selected_lines: tuple[ReconciliationLine, ...] | None = None,
    include_customers: bool = False,
) -> str:
    """Render one deterministic human-readable reconciliation report."""
    detail = summary.lines if selected_lines is None else selected_lines
    output = [
        "LedgerMatch",
        f"Procesadas: {len(summary.lines)}",
        f"Coinciden: {summary.matched}",
        f"Con diferencia: {summary.different}",
        f"Filas inválidas: {summary.invalid_rows}",
        f"Total facturado: {summary.invoice_total:.2f}",
        f"Total pagado: {summary.payment_total:.2f}",
        f"Detalle seleccionado: {len(detail)}",
    ]
    for line in detail:
        output.append(
            f"- {line.record.invoice_id} | {line.record.customer} | "
            f"{line.status.value} | diferencia {line.difference:+.2f}"
        )
    if include_customers:
        output.append("Por cliente:")
        for customer in summarize_by_customer(summary):
            output.append(
                f"- {customer.customer}: {customer.invoice_count} factura(s), "
                f"facturado {customer.invoice_total:.2f}, "
                f"pagado {customer.payment_total:.2f}, "
                f"diferencia {customer.difference:+.2f}"
            )
    return "\n".join(output)


def _open_destination(path: str | Path, *, force: bool, newline: str | None = None) -> TextIO:
    return Path(path).open("w" if force else "x", encoding="utf-8", newline=newline)


def _spreadsheet_safe_text(value: str) -> str:
    return "'" + value if value.startswith(_SPREADSHEET_FORMULA_PREFIXES) else value


def write_json_report(
    path: str | Path,
    summary: ReconciliationSummary,
    *,
    selected_lines: tuple[ReconciliationLine, ...] | None = None,
    force: bool = False,
) -> None:
    """Write a UTF-8 JSON report, preserving Decimal values as text."""
    detail = summary.lines if selected_lines is None else selected_lines
    payload = {
        "summary": {
            "processed": len(summary.lines),
            "matched": summary.matched,
            "different": summary.different,
            "invalid_rows": summary.invalid_rows,
            "invoice_total": str(summary.invoice_total),
            "payment_total": str(summary.payment_total),
        },
        "lines": [
            {
                "invoice_id": line.record.invoice_id,
                "customer": line.record.customer,
                "issued_on": line.record.issued_on.isoformat(),
                "invoice_total": str(line.record.invoice_total),
                "payment_total": str(line.record.payment_total),
                "status": line.status.value,
                "difference": str(line.difference),
            }
            for line in detail
        ],
    }
    with _open_destination(path, force=force) as stream:
        json.dump(payload, stream, ensure_ascii=False, indent=2)
        stream.write("\n")


def write_csv_report(
    path: str | Path,
    lines: tuple[ReconciliationLine, ...],
    *,
    force: bool = False,
) -> None:
    """Write selected detail as a spreadsheet-safer UTF-8 CSV report."""
    with _open_destination(path, force=force, newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["invoice_id", "customer", "issued_on", "invoice_total", "payment_total", "status", "difference"])
        for line in lines:
            writer.writerow([
                _spreadsheet_safe_text(line.record.invoice_id),
                _spreadsheet_safe_text(line.record.customer),
                line.record.issued_on.isoformat(),
                str(line.record.invoice_total),
                str(line.record.payment_total),
                line.status.value,
                str(line.difference),
            ])

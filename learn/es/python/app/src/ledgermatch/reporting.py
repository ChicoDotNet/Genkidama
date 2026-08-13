"""Format and export LedgerMatch reconciliation results."""

from __future__ import annotations

import csv
import json
from pathlib import Path

from .analytics import summarize_by_customer
from .reconciler import ReconciliationLine, ReconciliationSummary


def format_report(
    summary: ReconciliationSummary,
    *,
    selected_lines: tuple[ReconciliationLine, ...] | None = None,
    include_customers: bool = False,
) -> str:
    """Render one deterministic human-readable reconciliation report.

    Args:
        summary: Complete reconciliation result.
        selected_lines: Optional detail subset. The global summary remains based
            on the full reconciliation.
        include_customers: Include aggregate totals grouped by customer.

    Returns:
        Newline-delimited text. This function performs no I/O.
    """
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


def write_json_report(
    path: str | Path,
    summary: ReconciliationSummary,
    *,
    selected_lines: tuple[ReconciliationLine, ...] | None = None,
) -> None:
    """Write a UTF-8 JSON report with exact decimal values represented as text.

    Args:
        path: Destination file. Existing files are replaced in this lesson.
        summary: Complete reconciliation result.
        selected_lines: Optional detail subset to export.

    Raises:
        OSError: The destination cannot be written.

    Side Effects:
        Creates or replaces ``path``. Output order is deterministic for the same
        input summary and selected line order.
    """
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
    Path(path).write_text(
        json.dumps(payload, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )


def write_csv_report(
    path: str | Path,
    lines: tuple[ReconciliationLine, ...],
) -> None:
    """Write selected reconciliation detail as a UTF-8 CSV file.

    Args:
        path: Destination file. Existing files are replaced in this lesson.
        lines: Detail rows to serialize in their existing order.

    Raises:
        OSError: The destination cannot be written.

    Side Effects:
        Creates or replaces ``path`` using ``csv.writer`` for quoting rules.
    """
    with Path(path).open("w", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(
            [
                "invoice_id",
                "customer",
                "issued_on",
                "invoice_total",
                "payment_total",
                "status",
                "difference",
            ]
        )
        for line in lines:
            writer.writerow(
                [
                    line.record.invoice_id,
                    line.record.customer,
                    line.record.issued_on.isoformat(),
                    str(line.record.invoice_total),
                    str(line.record.payment_total),
                    line.status.value,
                    str(line.difference),
                ]
            )

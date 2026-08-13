"""Format LedgerMatch results as deterministic human-readable text."""

from __future__ import annotations

from .analytics import summarize_by_customer
from .reconciler import ReconciliationLine, ReconciliationSummary


def format_report(
    summary: ReconciliationSummary,
    *,
    selected_lines: tuple[ReconciliationLine, ...] | None = None,
    include_customers: bool = False,
) -> str:
    """Render one reconciliation summary without printing or reading files."""

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

from datetime import date
from decimal import Decimal

from ledgermatch import ParseResult, format_report, reconcile, select_lines
from ledgermatch.models import InvoiceRecord


def test_report_can_show_selected_detail_and_customer_totals():
    summary = reconcile(
        ParseResult(
            records=(
                InvoiceRecord("F-1", "Acme", date(2026, 8, 1), Decimal("100"), Decimal("100")),
                InvoiceRecord("F-2", "Acme", date(2026, 8, 2), Decimal("50"), Decimal("45")),
            ),
            issues=(),
        )
    )
    selected = select_lines(summary, only_differences=True)

    report = format_report(summary, selected_lines=selected, include_customers=True)

    assert "Detalle seleccionado: 1" in report
    assert "F-2 | Acme | difference | diferencia -5.00" in report
    assert "F-1 | Acme" not in report
    assert "Acme: 2 factura(s), facturado 150.00, pagado 145.00, diferencia -5.00" in report

from datetime import date
from decimal import Decimal

from ledgermatch import ParseResult, reconcile, select_lines, summarize_by_customer
from ledgermatch.models import InvoiceRecord


def _summary():
    return reconcile(
        ParseResult(
            records=(
                InvoiceRecord("F-1", "Acme", date(2026, 8, 1), Decimal("100"), Decimal("100")),
                InvoiceRecord("F-2", "ACME", date(2026, 8, 2), Decimal("50"), Decimal("45")),
                InvoiceRecord("F-3", "Beta", date(2026, 8, 3), Decimal("20"), Decimal("25")),
            ),
            issues=(),
        )
    )


def test_summarize_by_customer_groups_case_insensitively_and_orders_names():
    customers = summarize_by_customer(_summary())

    assert tuple(customer.customer for customer in customers) == ("Acme", "Beta")
    assert customers[0].invoice_count == 2
    assert customers[0].invoice_total == Decimal("150")
    assert customers[0].payment_total == Decimal("145")
    assert customers[0].difference == Decimal("-5")


def test_select_lines_combines_customer_and_difference_filters():
    selected = select_lines(_summary(), customer=" acme ", only_differences=True)

    assert tuple(line.record.invoice_id for line in selected) == ("F-2",)

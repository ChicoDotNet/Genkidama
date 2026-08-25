import 'package:flutter_test/flutter_test.dart';
import 'package:pocket_ledger/domain/expense.dart';
import 'package:pocket_ledger/domain/expense_ledger.dart';

void main() {
  test('ledger exposes immutable expenses and computes totals', () {
    final ledger = ExpenseLedger()
      ..add(
        Expense(
          description: 'Café',
          amountCents: 12550,
          category: ExpenseCategory.food,
          spentAt: DateTime.utc(2026, 8, 22),
        ),
      )
      ..add(
        Expense(
          description: 'Metro',
          amountCents: 750,
          category: ExpenseCategory.transport,
          spentAt: DateTime.utc(2026, 8, 22),
        ),
      );

    expect(ledger.totalCents, 13300);
    expect(ledger.totalsByCategory()[ExpenseCategory.food], 12550);
    expect(ledger.totalsByCategory()[ExpenseCategory.transport], 750);
    expect(
      () => ledger.expenses.add(ledger.expenses.first),
      throwsUnsupportedError,
    );
  });
}

import 'package:flutter_test/flutter_test.dart';
import 'package:pocket_ledger/domain/expense.dart';
import 'package:pocket_ledger/domain/expense_report.dart';

Expense expense({
  required int cents,
  required ExpenseCategory category,
  required DateTime spentAt,
}) {
  return Expense(
    description: 'fixture',
    amountCents: cents,
    category: category,
    spentAt: spentAt,
  );
}

void main() {
  test('builds a monthly report without crossing month boundaries', () {
    final report = ExpenseReport.forMonth([
      expense(
        cents: 1000,
        category: ExpenseCategory.food,
        spentAt: DateTime(2026, 8, 1),
      ),
      expense(
        cents: 2500,
        category: ExpenseCategory.transport,
        spentAt: DateTime(2026, 8, 31, 23, 59),
      ),
      expense(
        cents: 9000,
        category: ExpenseCategory.home,
        spentAt: DateTime(2026, 9, 1),
      ),
    ], DateTime(2026, 8, 20));

    expect(report.count, 2);
    expect(report.totalCents, 3500);
    expect(report.totalsByCategory[ExpenseCategory.food], 1000);
    expect(report.totalsByCategory[ExpenseCategory.transport], 2500);
    expect(report.totalsByCategory.containsKey(ExpenseCategory.home), isFalse);
  });

  test('uses an exclusive end boundary to prevent double counting', () {
    final boundary = expense(
      cents: 700,
      category: ExpenseCategory.other,
      spentAt: DateTime(2026, 9, 1),
    );

    final august = ExpenseReport.between(
      [boundary],
      startInclusive: DateTime(2026, 8, 1),
      endExclusive: DateTime(2026, 9, 1),
    );
    final september = ExpenseReport.between(
      [boundary],
      startInclusive: DateTime(2026, 9, 1),
      endExclusive: DateTime(2026, 10, 1),
    );

    expect(august.count, 0);
    expect(september.count, 1);
  });

  test('rejects an empty or inverted report interval', () {
    expect(
      () => ExpenseReport.between(
        const <Expense>[],
        startInclusive: DateTime(2026, 8, 1),
        endExclusive: DateTime(2026, 8, 1),
      ),
      throwsArgumentError,
    );
  });
}

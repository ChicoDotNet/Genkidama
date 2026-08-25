import 'package:flutter_test/flutter_test.dart';
import 'package:pocket_ledger/domain/expense.dart';

void main() {
  group('Expense', () {
    test('normalizes description and preserves integer cents', () {
      final expense = Expense(
        description: '  Café  ',
        amountCents: 12550,
        category: ExpenseCategory.food,
        spentAt: DateTime.utc(2026, 8, 22),
      );

      expect(expense.description, 'Café');
      expect(expense.amountCents, 12550);
      expect(expense.amountDisplay, '125.50');
    });

    test('rejects blank descriptions', () {
      expect(
        () => Expense(
          description: '   ',
          amountCents: 100,
          category: ExpenseCategory.other,
          spentAt: DateTime.utc(2026, 8, 22),
        ),
        throwsArgumentError,
      );
    });

    test('rejects zero or negative amounts', () {
      expect(
        () => Expense(
          description: 'Prueba',
          amountCents: 0,
          category: ExpenseCategory.other,
          spentAt: DateTime.utc(2026, 8, 22),
        ),
        throwsArgumentError,
      );
    });
  });
}

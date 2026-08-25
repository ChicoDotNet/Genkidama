import 'package:flutter_test/flutter_test.dart';
import 'package:pocket_ledger/application/expense_controller.dart';
import 'package:pocket_ledger/domain/expense.dart';
import 'package:pocket_ledger/persistence/expense_store.dart';

import 'support/memory_expense_store.dart';

void main() {
  Expense sampleExpense(String description, int cents) => Expense(
    description: description,
    amountCents: cents,
    category: ExpenseCategory.other,
    spentAt: DateTime.utc(2026, 8, 22),
  );

  test('load publishes persisted expenses and a ready state', () async {
    final store = MemoryExpenseStore(
      initialExpenses: <Expense>[sampleExpense('Libro', 25000)],
    );
    final controller = ExpenseController(store: store);

    await controller.load();

    expect(controller.state, ExpenseLoadState.ready);
    expect(controller.expenses.single.description, 'Libro');
    expect(controller.totalCents, 25000);
  });

  test(
    'load exposes a persistence failure without inventing empty data',
    () async {
      final controller = ExpenseController(
        store: MemoryExpenseStore(failLoad: true),
      );

      await controller.load();

      expect(controller.state, ExpenseLoadState.failed);
      expect(controller.errorMessage, contains('fallo de lectura'));
      expect(controller.expenses, isEmpty);
    },
  );

  test('add persists before publishing the new expense', () async {
    final store = MemoryExpenseStore();
    final controller = ExpenseController(store: store);
    await controller.load();

    await controller.addExpense(sampleExpense('Taxi', 8900));

    expect(store.saved, hasLength(1));
    expect(controller.expenses.single.description, 'Taxi');
  });

  test('failed save leaves the visible ledger unchanged', () async {
    final initial = sampleExpense('Inicial', 1000);
    final store = MemoryExpenseStore(
      initialExpenses: <Expense>[initial],
      failSave: true,
    );
    final controller = ExpenseController(store: store);
    await controller.load();

    await expectLater(
      controller.addExpense(sampleExpense('No guardado', 2000)),
      throwsA(isA<ExpenseStoreException>()),
    );

    expect(controller.expenses, hasLength(1));
    expect(controller.expenses.single.description, 'Inicial');
  });

  test('diagnostics expose aggregates without leaking descriptions', () async {
    final first = sampleExpense('Secreto A', 1000);
    final second = sampleExpense('Secreto B', 2000);
    final controller = ExpenseController(
      store: MemoryExpenseStore(initialExpenses: <Expense>[first, second]),
    );
    await controller.load();

    final diagnostics = controller.diagnostics();

    expect(diagnostics.state, ExpenseLoadState.ready);
    expect(diagnostics.expenseCount, 2);
    expect(diagnostics.totalCents, 3000);
    expect(diagnostics.categoryCounts[ExpenseCategory.other], 2);
    expect(diagnostics.toString(), isNot(contains('Secreto')));
  });
}

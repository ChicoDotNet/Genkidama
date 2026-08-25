import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:pocket_ledger/application/expense_controller.dart';
import 'package:pocket_ledger/domain/expense.dart';
import 'package:pocket_ledger/main.dart';

import 'support/memory_expense_store.dart';

void main() {
  Future<ExpenseController> readyController({
    Iterable<Expense> initialExpenses = const <Expense>[],
    bool failLoad = false,
    bool failSave = false,
  }) async {
    final controller = ExpenseController(
      store: MemoryExpenseStore(
        initialExpenses: initialExpenses,
        failLoad: failLoad,
        failSave: failSave,
      ),
    );
    await controller.load();
    return controller;
  }

  testWidgets('adds, persists and displays an expense', (tester) async {
    final controller = await readyController();
    await tester.pumpWidget(PocketLedgerApp(controller: controller));

    expect(find.text('No hay gastos para este filtro.'), findsOneWidget);
    expect(find.text('Total: \$0.00'), findsOneWidget);

    await tester.tap(
      find.widgetWithText(FloatingActionButton, 'Agregar gasto'),
    );
    await tester.pumpAndSettle();

    await tester.enterText(
      find.byKey(const Key('expense-description')),
      'Café',
    );
    await tester.enterText(find.byKey(const Key('expense-amount')), '125.50');
    await tester.tap(find.widgetWithText(FilledButton, 'Guardar'));
    await tester.pumpAndSettle();

    expect(find.text('Café'), findsOneWidget);
    expect(find.text('\$125.50'), findsOneWidget);
    expect(find.text('Total: \$125.50'), findsOneWidget);
    expect(controller.expenses, hasLength(1));
  });

  testWidgets('shows current-month summary and filters by category', (
    tester,
  ) async {
    final now = DateTime.now();
    final controller = await readyController(
      initialExpenses: [
        Expense(
          description: 'Comida',
          amountCents: 1200,
          category: ExpenseCategory.food,
          spentAt: now,
        ),
        Expense(
          description: 'Taxi',
          amountCents: 800,
          category: ExpenseCategory.transport,
          spentAt: now,
        ),
      ],
    );
    await tester.pumpWidget(PocketLedgerApp(controller: controller));

    expect(find.text('Este mes: \$20.00 · 2 gasto(s)'), findsOneWidget);
    expect(find.text('Comida'), findsOneWidget);
    expect(find.text('Taxi'), findsOneWidget);

    await tester.tap(find.byKey(const Key('category-filter')));
    await tester.pumpAndSettle();
    await tester.tap(find.text('food').last);
    await tester.pumpAndSettle();

    expect(find.text('Comida'), findsOneWidget);
    expect(find.text('Taxi'), findsNothing);
  });

  testWidgets('keeps the dialog open for an invalid amount', (tester) async {
    final controller = await readyController();
    await tester.pumpWidget(PocketLedgerApp(controller: controller));
    await tester.tap(
      find.widgetWithText(FloatingActionButton, 'Agregar gasto'),
    );
    await tester.pumpAndSettle();

    await tester.enterText(
      find.byKey(const Key('expense-description')),
      'Café',
    );
    await tester.enterText(find.byKey(const Key('expense-amount')), 'abc');
    await tester.tap(find.widgetWithText(FilledButton, 'Guardar'));
    await tester.pump();

    expect(find.text('Escribe un monto válido.'), findsOneWidget);
    expect(find.byType(AlertDialog), findsOneWidget);
  });

  testWidgets('shows a load failure and disables writes', (tester) async {
    final controller = await readyController(failLoad: true);

    await tester.pumpWidget(PocketLedgerApp(controller: controller));

    expect(find.byKey(const Key('persistence-error')), findsOneWidget);
    expect(find.byKey(const Key('retry-load')), findsOneWidget);
    expect(find.byType(FloatingActionButton), findsNothing);
  });

  testWidgets('keeps the dialog open when persistence fails', (tester) async {
    final controller = await readyController(failSave: true);
    await tester.pumpWidget(PocketLedgerApp(controller: controller));
    await tester.tap(
      find.widgetWithText(FloatingActionButton, 'Agregar gasto'),
    );
    await tester.pumpAndSettle();

    await tester.enterText(
      find.byKey(const Key('expense-description')),
      'Taxi',
    );
    await tester.enterText(find.byKey(const Key('expense-amount')), '50');
    await tester.tap(find.widgetWithText(FilledButton, 'Guardar'));
    await tester.pump();

    expect(find.textContaining('fallo de escritura'), findsOneWidget);
    expect(find.byType(AlertDialog), findsOneWidget);
    expect(controller.expenses, isEmpty);
  });
}

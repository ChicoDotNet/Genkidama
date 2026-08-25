import 'dart:collection';

import 'expense.dart';

/// In-memory expense collection used by PocketLedger domain calculations.
///
/// The ledger owns its mutable list and exposes an unmodifiable view. Storage
/// and Flutter widgets remain outside this domain object.
class ExpenseLedger {
  /// Creates a ledger with an optional validated initial sequence.
  ExpenseLedger([Iterable<Expense> initialExpenses = const <Expense>[]])
    : _expenses = List<Expense>.of(initialExpenses);

  final List<Expense> _expenses;

  /// Expenses in insertion order. Callers cannot mutate the internal list.
  UnmodifiableListView<Expense> get expenses => UnmodifiableListView(_expenses);

  /// Total amount in cents for all recorded expenses.
  int get totalCents =>
      _expenses.fold(0, (sum, expense) => sum + expense.amountCents);

  /// Adds an already validated [expense].
  void add(Expense expense) => _expenses.add(expense);

  /// Totals expenses per category using integer cents.
  Map<ExpenseCategory, int> totalsByCategory() {
    final totals = <ExpenseCategory, int>{};
    for (final expense in _expenses) {
      totals.update(
        expense.category,
        (value) => value + expense.amountCents,
        ifAbsent: () => expense.amountCents,
      );
    }
    return Map.unmodifiable(totals);
  }
}

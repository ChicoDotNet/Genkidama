import 'dart:collection';

import 'package:flutter/foundation.dart';

import '../domain/expense.dart';
import '../domain/expense_ledger.dart';
import '../domain/expense_report.dart';
import '../persistence/expense_store.dart';

/// Observable lifecycle for the PocketLedger application state.
enum ExpenseLoadState { idle, loading, ready, failed }

/// Privacy-conscious operational snapshot for debugging PocketLedger.
///
/// It intentionally contains counts, totals and lifecycle state, but never
/// expense descriptions or other user-entered text.
class ExpenseDiagnostics {
  /// Creates one immutable diagnostics snapshot.
  ExpenseDiagnostics({
    required this.state,
    required this.expenseCount,
    required this.totalCents,
    required Map<ExpenseCategory, int> categoryCounts,
  }) : categoryCounts = UnmodifiableMapView(categoryCounts);

  /// Application lifecycle when the snapshot was produced.
  final ExpenseLoadState state;

  /// Number of expenses currently visible to the application.
  final int expenseCount;

  /// Aggregate amount in integer cents.
  final int totalCents;

  /// Immutable counts grouped by category, without user-entered descriptions.
  final UnmodifiableMapView<ExpenseCategory, int> categoryCounts;
}

/// Application-state boundary between Flutter widgets, domain and persistence.
///
/// The controller persists a candidate snapshot before exposing a new expense,
/// so a failed write cannot make the visible state disagree with disk.
class ExpenseController extends ChangeNotifier {
  /// Creates a controller backed by [store]. No I/O happens until [load].
  ExpenseController({required ExpenseStore store}) : _store = store;

  final ExpenseStore _store;
  ExpenseLedger _ledger = ExpenseLedger();
  ExpenseLoadState _state = ExpenseLoadState.idle;
  String? _errorMessage;

  /// Current ordered expenses as an unmodifiable view.
  UnmodifiableListView<Expense> get expenses => _ledger.expenses;

  /// Current total in integer cents.
  int get totalCents => _ledger.totalCents;

  /// Current loading lifecycle.
  ExpenseLoadState get state => _state;

  /// Presentable failure message when [state] is [ExpenseLoadState.failed].
  String? get errorMessage => _errorMessage;

  /// Returns the current expenses filtered by [category], or all when null.
  List<Expense> expensesForCategory(ExpenseCategory? category) {
    if (category == null) {
      return List<Expense>.unmodifiable(_ledger.expenses);
    }
    return List<Expense>.unmodifiable(
      _ledger.expenses.where((expense) => expense.category == category),
    );
  }

  /// Builds a read-only calendar-month report without mutating application state.
  ExpenseReport reportForMonth(DateTime month) {
    return ExpenseReport.forMonth(_ledger.expenses, month);
  }

  /// Returns a deterministic diagnostics snapshot without user-entered text.
  ExpenseDiagnostics diagnostics() {
    final counts = <ExpenseCategory, int>{};
    for (final expense in _ledger.expenses) {
      counts.update(expense.category, (value) => value + 1, ifAbsent: () => 1);
    }
    return ExpenseDiagnostics(
      state: _state,
      expenseCount: _ledger.expenses.length,
      totalCents: _ledger.totalCents,
      categoryCounts: counts,
    );
  }

  /// Loads persisted state, replacing memory only after a successful read.
  Future<void> load() async {
    _state = ExpenseLoadState.loading;
    _errorMessage = null;
    notifyListeners();

    try {
      final expenses = await _store.load();
      _ledger = ExpenseLedger(expenses);
      _state = ExpenseLoadState.ready;
    } on ExpenseStoreException catch (error) {
      _state = ExpenseLoadState.failed;
      _errorMessage = error.message;
    }
    notifyListeners();
  }

  /// Retries the persisted read after an earlier load failure.
  Future<void> retryLoad() => load();

  /// Persists and then publishes one validated [expense].
  ///
  /// Throws [ExpenseStoreException] when persistence fails. In that case the
  /// visible ledger remains unchanged.
  Future<void> addExpense(Expense expense) async {
    final candidate = <Expense>[..._ledger.expenses, expense];
    await _store.save(candidate);
    _ledger = ExpenseLedger(candidate);
    _state = ExpenseLoadState.ready;
    _errorMessage = null;
    notifyListeners();
  }
}

import 'dart:collection';

import 'expense.dart';

/// Read-only summary of expenses inside one half-open time interval.
///
/// The interval is `[startInclusive, endExclusive)`, which avoids double-counting
/// expenses that fall exactly on a month boundary.
class ExpenseReport {
  ExpenseReport._({
    required this.startInclusive,
    required this.endExclusive,
    required this.count,
    required this.totalCents,
    required Map<ExpenseCategory, int> totalsByCategory,
  }) : totalsByCategory = UnmodifiableMapView(totalsByCategory);

  /// Creates a deterministic report from [expenses] for the requested range.
  factory ExpenseReport.between(
    Iterable<Expense> expenses, {
    required DateTime startInclusive,
    required DateTime endExclusive,
  }) {
    if (!endExclusive.isAfter(startInclusive)) {
      throw ArgumentError.value(
        endExclusive,
        'endExclusive',
        'Debe ser posterior a startInclusive.',
      );
    }

    var count = 0;
    var totalCents = 0;
    final totals = <ExpenseCategory, int>{};

    for (final expense in expenses) {
      final inside =
          !expense.spentAt.isBefore(startInclusive) &&
          expense.spentAt.isBefore(endExclusive);
      if (!inside) {
        continue;
      }

      count += 1;
      totalCents += expense.amountCents;
      totals.update(
        expense.category,
        (value) => value + expense.amountCents,
        ifAbsent: () => expense.amountCents,
      );
    }

    return ExpenseReport._(
      startInclusive: startInclusive,
      endExclusive: endExclusive,
      count: count,
      totalCents: totalCents,
      totalsByCategory: totals,
    );
  }

  /// Creates a report for the calendar month containing [month].
  factory ExpenseReport.forMonth(Iterable<Expense> expenses, DateTime month) {
    final start = DateTime(month.year, month.month);
    final end = month.month == 12
        ? DateTime(month.year + 1, 1)
        : DateTime(month.year, month.month + 1);
    return ExpenseReport.between(
      expenses,
      startInclusive: start,
      endExclusive: end,
    );
  }

  /// Inclusive beginning of the report window.
  final DateTime startInclusive;

  /// Exclusive end of the report window.
  final DateTime endExclusive;

  /// Number of expenses included in the report.
  final int count;

  /// Sum of all included amounts in integer cents.
  final int totalCents;

  /// Immutable category totals for the same report interval.
  final UnmodifiableMapView<ExpenseCategory, int> totalsByCategory;
}

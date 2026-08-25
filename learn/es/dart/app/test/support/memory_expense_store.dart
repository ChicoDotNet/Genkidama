import 'package:pocket_ledger/domain/expense.dart';
import 'package:pocket_ledger/persistence/expense_store.dart';

class MemoryExpenseStore implements ExpenseStore {
  MemoryExpenseStore({
    Iterable<Expense> initialExpenses = const <Expense>[],
    this.failLoad = false,
    this.failSave = false,
  }) : saved = List<Expense>.of(initialExpenses);

  List<Expense> saved;
  bool failLoad;
  bool failSave;

  @override
  Future<List<Expense>> load() async {
    if (failLoad) {
      throw const ExpenseStoreException('fallo de lectura simulado');
    }
    return List<Expense>.of(saved);
  }

  @override
  Future<void> save(List<Expense> expenses) async {
    if (failSave) {
      throw const ExpenseStoreException('fallo de escritura simulado');
    }
    saved = List<Expense>.of(expenses);
  }
}

import 'dart:io';

import 'package:flutter/material.dart';
import 'package:path_provider/path_provider.dart';

import 'application/expense_controller.dart';
import 'domain/expense.dart';
import 'persistence/expense_store.dart';

Future<void> main() async {
  WidgetsFlutterBinding.ensureInitialized();
  final directory = await getApplicationDocumentsDirectory();
  final store = JsonFileExpenseStore(
    File('${directory.path}${Platform.pathSeparator}pocket_ledger.json'),
  );
  final controller = ExpenseController(store: store);
  await controller.load();
  runApp(PocketLedgerApp(controller: controller));
}

/// Root widget for the PocketLedger learning application.
class PocketLedgerApp extends StatelessWidget {
  /// Creates the application with an explicit [controller] boundary.
  const PocketLedgerApp({required this.controller, super.key});

  /// Application state shared with the home screen.
  final ExpenseController controller;

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'PocketLedger',
      theme: ThemeData(useMaterial3: true),
      home: ExpenseHomePage(controller: controller),
    );
  }
}

/// Home screen that observes a persisted [ExpenseController].
class ExpenseHomePage extends StatefulWidget {
  /// Creates the home screen for [controller].
  const ExpenseHomePage({required this.controller, super.key});

  /// Application-state boundary used by this screen.
  final ExpenseController controller;

  @override
  State<ExpenseHomePage> createState() => _ExpenseHomePageState();
}

class _ExpenseHomePageState extends State<ExpenseHomePage> {
  ExpenseCategory? _categoryFilter;

  String _money(int cents) => '\$${(cents / 100).toStringAsFixed(2)}';

  @override
  void initState() {
    super.initState();
    widget.controller.addListener(_refresh);
  }

  @override
  void didUpdateWidget(covariant ExpenseHomePage oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (oldWidget.controller != widget.controller) {
      oldWidget.controller.removeListener(_refresh);
      widget.controller.addListener(_refresh);
    }
  }

  @override
  void dispose() {
    widget.controller.removeListener(_refresh);
    super.dispose();
  }

  void _refresh() {
    if (mounted) {
      setState(() {});
    }
  }

  Future<void> _openAddExpenseDialog() async {
    var description = '';
    var amountText = '';
    var category = ExpenseCategory.food;
    String? errorMessage;

    await showDialog<void>(
      context: context,
      builder: (context) {
        return StatefulBuilder(
          builder: (context, setDialogState) {
            return AlertDialog(
              title: const Text('Agregar gasto'),
              content: SingleChildScrollView(
                child: Column(
                  mainAxisSize: MainAxisSize.min,
                  children: [
                    TextField(
                      key: const Key('expense-description'),
                      onChanged: (value) => description = value,
                      decoration: const InputDecoration(
                        labelText: 'Descripción',
                      ),
                    ),
                    TextField(
                      key: const Key('expense-amount'),
                      onChanged: (value) => amountText = value,
                      keyboardType: const TextInputType.numberWithOptions(
                        decimal: true,
                      ),
                      decoration: const InputDecoration(labelText: 'Monto'),
                    ),
                    DropdownButtonFormField<ExpenseCategory>(
                      initialValue: category,
                      decoration: const InputDecoration(labelText: 'Categoría'),
                      items: ExpenseCategory.values
                          .map(
                            (value) => DropdownMenuItem(
                              value: value,
                              child: Text(value.name),
                            ),
                          )
                          .toList(),
                      onChanged: (value) {
                        if (value != null) {
                          setDialogState(() => category = value);
                        }
                      },
                    ),
                    if (errorMessage != null)
                      Padding(
                        padding: const EdgeInsets.only(top: 12),
                        child: Text(
                          errorMessage!,
                          key: const Key('expense-error'),
                        ),
                      ),
                  ],
                ),
              ),
              actions: [
                TextButton(
                  onPressed: () => Navigator.of(context).pop(),
                  child: const Text('Cancelar'),
                ),
                FilledButton(
                  onPressed: () async {
                    final amount = double.tryParse(amountText.trim());
                    if (amount == null) {
                      setDialogState(
                        () => errorMessage = 'Escribe un monto válido.',
                      );
                      return;
                    }

                    try {
                      final expense = Expense(
                        description: description,
                        amountCents: (amount * 100).round(),
                        category: category,
                        spentAt: DateTime.now(),
                      );
                      await widget.controller.addExpense(expense);
                      if (context.mounted) {
                        Navigator.of(context).pop();
                      }
                    } on ArgumentError catch (error) {
                      setDialogState(
                        () => errorMessage = error.message.toString(),
                      );
                    } on ExpenseStoreException catch (error) {
                      setDialogState(() => errorMessage = error.message);
                    }
                  },
                  child: const Text('Guardar'),
                ),
              ],
            );
          },
        );
      },
    );
  }

  @override
  Widget build(BuildContext context) {
    final controller = widget.controller;
    final persistenceFailed = controller.state == ExpenseLoadState.failed;
    final monthlyReport = controller.reportForMonth(DateTime.now());
    final visibleExpenses = controller.expensesForCategory(_categoryFilter);

    return Scaffold(
      appBar: AppBar(title: const Text('PocketLedger')),
      body: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.stretch,
          children: [
            if (persistenceFailed)
              Card(
                child: Padding(
                  padding: const EdgeInsets.all(16),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        controller.errorMessage ??
                            'No se pudieron cargar los datos.',
                        key: const Key('persistence-error'),
                      ),
                      const SizedBox(height: 8),
                      OutlinedButton(
                        key: const Key('retry-load'),
                        onPressed: controller.retryLoad,
                        child: const Text('Reintentar'),
                      ),
                    ],
                  ),
                ),
              ),
            Card(
              child: Padding(
                padding: const EdgeInsets.all(16),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      'Total: ${_money(controller.totalCents)}',
                      key: const Key('expense-total'),
                      style: Theme.of(context).textTheme.headlineSmall,
                    ),
                    const SizedBox(height: 6),
                    Text(
                      'Este mes: ${_money(monthlyReport.totalCents)} · '
                      '${monthlyReport.count} gasto(s)',
                      key: const Key('month-report'),
                    ),
                  ],
                ),
              ),
            ),
            const SizedBox(height: 12),
            DropdownButtonFormField<ExpenseCategory?>(
              key: const Key('category-filter'),
              initialValue: _categoryFilter,
              decoration: const InputDecoration(labelText: 'Filtrar categoría'),
              items: [
                const DropdownMenuItem<ExpenseCategory?>(
                  value: null,
                  child: Text('Todas'),
                ),
                ...ExpenseCategory.values.map(
                  (category) => DropdownMenuItem<ExpenseCategory?>(
                    value: category,
                    child: Text(category.name),
                  ),
                ),
              ],
              onChanged: (value) => setState(() => _categoryFilter = value),
            ),
            const SizedBox(height: 12),
            Expanded(
              child: visibleExpenses.isEmpty
                  ? const Center(child: Text('No hay gastos para este filtro.'))
                  : ListView.builder(
                      itemCount: visibleExpenses.length,
                      itemBuilder: (context, index) {
                        final expense = visibleExpenses[index];
                        return ListTile(
                          title: Text(expense.description),
                          subtitle: Text(expense.category.name),
                          trailing: Text(_money(expense.amountCents)),
                        );
                      },
                    ),
            ),
          ],
        ),
      ),
      floatingActionButton: persistenceFailed
          ? null
          : FloatingActionButton.extended(
              onPressed: _openAddExpenseDialog,
              icon: const Icon(Icons.add),
              label: const Text('Agregar gasto'),
            ),
    );
  }
}

import 'dart:convert';
import 'dart:io';

import '../domain/expense.dart';

/// Persistence boundary for PocketLedger expenses.
abstract interface class ExpenseStore {
  /// Loads all persisted expenses in their stored order.
  ///
  /// Throws [ExpenseStoreException] when persisted data cannot be read safely.
  Future<List<Expense>> load();

  /// Persists the complete ordered [expenses] snapshot.
  ///
  /// Throws [ExpenseStoreException] when the snapshot cannot be written.
  Future<void> save(List<Expense> expenses);
}

/// Actionable failure raised by the persistence boundary.
class ExpenseStoreException implements Exception {
  /// Creates a persistence error with optional underlying [cause].
  const ExpenseStoreException(this.message, [this.cause]);

  /// Human-readable diagnostic safe to present to the learner/user.
  final String message;

  /// Original failure retained for diagnostics without changing the contract.
  final Object? cause;

  @override
  String toString() => 'ExpenseStoreException: $message';
}

/// JSON-file implementation with a versioned document envelope.
///
/// The caller owns the file location. A missing file means a new empty ledger;
/// malformed or unsupported persisted data is never silently discarded.
class JsonFileExpenseStore implements ExpenseStore {
  /// Creates a store that reads and writes [file].
  JsonFileExpenseStore(this.file);

  /// Current on-disk schema understood by this course increment.
  static const int schemaVersion = 1;

  /// File used for persistence.
  final File file;

  @override
  Future<List<Expense>> load() async {
    if (!await file.exists()) {
      return <Expense>[];
    }

    try {
      final decoded = jsonDecode(await file.readAsString());
      if (decoded is! Map<String, dynamic>) {
        throw const FormatException('La raíz debe ser un objeto JSON.');
      }
      if (decoded['schemaVersion'] != schemaVersion) {
        throw FormatException(
          'Versión de datos no soportada: ${decoded['schemaVersion']}.',
        );
      }

      final rawExpenses = decoded['expenses'];
      if (rawExpenses is! List<dynamic>) {
        throw const FormatException('La colección expenses es inválida.');
      }

      return rawExpenses
          .map((item) {
            if (item is! Map<String, dynamic>) {
              throw const FormatException(
                'Un gasto persistido no es un objeto.',
              );
            }
            return Expense.fromJson(item);
          })
          .toList(growable: false);
    } on ExpenseStoreException {
      rethrow;
    } on Object catch (error) {
      throw ExpenseStoreException(
        'No se pudo leer ${file.path} sin perder datos.',
        error,
      );
    }
  }

  @override
  Future<void> save(List<Expense> expenses) async {
    final temporary = File('${file.path}.tmp');
    final document = <String, Object>{
      'schemaVersion': schemaVersion,
      'expenses': expenses.map((expense) => expense.toJson()).toList(),
    };

    try {
      await file.parent.create(recursive: true);
      await temporary.writeAsString(jsonEncode(document), flush: true);
      if (await file.exists()) {
        await file.delete();
      }
      await temporary.rename(file.path);
    } on Object catch (error) {
      if (await temporary.exists()) {
        await temporary.delete();
      }
      throw ExpenseStoreException(
        'No se pudo guardar ${file.path}. El estado en memoria no cambió.',
        error,
      );
    }
  }
}

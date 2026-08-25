import 'dart:io';

import 'package:flutter_test/flutter_test.dart';
import 'package:pocket_ledger/domain/expense.dart';
import 'package:pocket_ledger/persistence/expense_store.dart';

void main() {
  late Directory temporaryDirectory;
  late File dataFile;

  setUp(() async {
    temporaryDirectory = await Directory.systemTemp.createTemp(
      'pocket-ledger-',
    );
    dataFile = File(
      '${temporaryDirectory.path}${Platform.pathSeparator}data.json',
    );
  });

  tearDown(() async {
    if (await temporaryDirectory.exists()) {
      await temporaryDirectory.delete(recursive: true);
    }
  });

  test('missing file loads an empty ledger', () async {
    final store = JsonFileExpenseStore(dataFile);

    expect(await store.load(), isEmpty);
  });

  test('round-trips validated expenses through versioned JSON', () async {
    final store = JsonFileExpenseStore(dataFile);
    final original = Expense(
      description: 'Supermercado',
      amountCents: 45990,
      category: ExpenseCategory.food,
      spentAt: DateTime.utc(2026, 8, 22, 12, 30),
    );

    await store.save(<Expense>[original]);
    final loaded = await store.load();

    expect(loaded, hasLength(1));
    expect(loaded.single.description, 'Supermercado');
    expect(loaded.single.amountCents, 45990);
    expect(loaded.single.category, ExpenseCategory.food);
    expect(loaded.single.spentAt, DateTime.utc(2026, 8, 22, 12, 30));
  });

  test(
    'rejects malformed persisted data instead of silently resetting',
    () async {
      await dataFile.writeAsString('{not-json');
      final store = JsonFileExpenseStore(dataFile);

      await expectLater(store.load(), throwsA(isA<ExpenseStoreException>()));
    },
  );

  test('rejects an unsupported schema version', () async {
    await dataFile.writeAsString('{"schemaVersion":99,"expenses":[]}');
    final store = JsonFileExpenseStore(dataFile);

    await expectLater(store.load(), throwsA(isA<ExpenseStoreException>()));
  });
}

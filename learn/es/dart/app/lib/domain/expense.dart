/// Categories used to group expenses without depending on UI labels.
enum ExpenseCategory { food, transport, home, health, other }

/// Immutable personal expense represented in integer cents.
///
/// Throws [ArgumentError] when [description] is blank or [amountCents] is not
/// positive. Construction has no I/O side effects and is deterministic.
class Expense {
  Expense({
    required String description,
    required int amountCents,
    required this.category,
    required this.spentAt,
  }) : description = _normalizeDescription(description),
       amountCents = _validateAmount(amountCents);

  /// Builds a validated expense from its persisted JSON representation.
  ///
  /// Throws [FormatException] when required fields are missing, have the wrong
  /// type, contain an unknown category or violate the normal domain rules.
  factory Expense.fromJson(Map<String, dynamic> json) {
    final description = json['description'];
    final amountCents = json['amountCents'];
    final categoryName = json['category'];
    final spentAtText = json['spentAt'];

    if (description is! String ||
        amountCents is! int ||
        categoryName is! String ||
        spentAtText is! String) {
      throw const FormatException('El gasto persistido tiene tipos inválidos.');
    }

    final category = _categoryFromName(categoryName);
    final spentAt = DateTime.tryParse(spentAtText);
    if (spentAt == null) {
      throw FormatException('Fecha de gasto inválida: $spentAtText');
    }

    try {
      return Expense(
        description: description,
        amountCents: amountCents,
        category: category,
        spentAt: spentAt,
      );
    } on ArgumentError catch (error) {
      throw FormatException('Gasto persistido inválido: ${error.message}');
    }
  }

  /// Human-readable description with surrounding whitespace removed.
  final String description;

  /// Amount expressed in the smallest currency unit to avoid binary rounding.
  final int amountCents;

  /// User-selected category.
  final ExpenseCategory category;

  /// Date/time supplied by the application boundary.
  final DateTime spentAt;

  /// Amount formatted with two decimal places for the current simple UI.
  String get amountDisplay => (amountCents / 100).toStringAsFixed(2);

  /// Returns a deterministic JSON-safe representation for local persistence.
  Map<String, Object> toJson() => <String, Object>{
    'description': description,
    'amountCents': amountCents,
    'category': category.name,
    'spentAt': spentAt.toUtc().toIso8601String(),
  };

  static ExpenseCategory _categoryFromName(String name) {
    for (final category in ExpenseCategory.values) {
      if (category.name == name) {
        return category;
      }
    }
    throw FormatException('Categoría de gasto desconocida: $name');
  }

  static String _normalizeDescription(String value) {
    final normalized = value.trim();
    if (normalized.isEmpty) {
      throw ArgumentError.value(value, 'description', 'No puede estar vacía.');
    }
    return normalized;
  }

  static int _validateAmount(int value) {
    if (value <= 0) {
      throw ArgumentError.value(
        value,
        'amountCents',
        'Debe ser mayor que cero.',
      );
    }
    return value;
  }
}

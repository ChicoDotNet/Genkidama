class Registry {
  Registry._();

  static final Registry _instance = Registry._();

  factory Registry() => _instance;

  int count = 0;
}

void main() {
  final first = Registry();
  final second = Registry();
  first.count += 1;
  print('same=${identical(first, second)}');
  print('count=${second.count}');
}

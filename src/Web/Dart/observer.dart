typedef Observer = String Function(int id);

final class Subject {
  final Map<String, Observer> _observers = <String, Observer>{};

  bool subscribe(String key, Observer observer) {
    if (_observers.containsKey(key)) return false;
    _observers[key] = observer;
    return true;
  }

  bool unsubscribe(String key) => _observers.remove(key) != null;

  List<String> publish(int id) => <String>[
    for (final observer in _observers.values) observer(id),
  ];
}

bool observerExamplePasses() {
  final subject = Subject();
  final auditAdded = subject.subscribe('audit', (id) => 'audit:$id');
  final dashboardAdded = subject.subscribe(
    'dashboard',
    (id) => 'dashboard:$id',
  );
  final duplicateRejected = !subject.subscribe(
    'audit',
    (id) => 'duplicate:$id',
  );

  final first = subject.publish(42).join('>');
  final dashboardRemoved = subject.unsubscribe('dashboard');
  final second = subject.publish(43).join('>');

  return auditAdded &&
      dashboardAdded &&
      duplicateRejected &&
      first == 'audit:42>dashboard:42' &&
      dashboardRemoved &&
      second == 'audit:43';
}

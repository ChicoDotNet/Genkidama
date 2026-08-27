abstract interface class DocumentStore {
  String fetch(int id);
}

final class DocumentBackend implements DocumentStore {
  int fetches = 0;

  @override
  String fetch(int id) {
    fetches += 1;
    return 'doc($id)';
  }
}

final class DocumentProxy implements DocumentStore {
  DocumentBackend? _backend;
  final Map<int, String> _cache = <int, String>{};

  int get backendCount => _backend == null ? 0 : 1;
  int get fetches => _backend?.fetches ?? 0;

  @override
  String fetch(int id) {
    return _cache.putIfAbsent(id, () {
      _backend ??= DocumentBackend();
      return _backend!.fetch(id);
    });
  }
}

void main() {
  final proxy = DocumentProxy();
  final first = proxy.fetch(42);
  final second = proxy.fetch(42);
  print(
    'backend=${proxy.backendCount};fetches=${proxy.fetches};first=$first;second=$second',
  );
}

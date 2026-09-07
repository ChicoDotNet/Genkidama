class CursorIterator<T> {
  CursorIterator(this.values);

  final List<T> values;
  int _index = 0;

  bool get hasNext => _index < values.length;
  T next() => values[_index++];
}

void main() {
  final iterator = CursorIterator<int>([10, 20, 30]);
  final visited = <int>[];
  while (iterator.hasNext) {
    visited.add(iterator.next());
  }

  if (visited.join(',') != '10,20,30' || iterator.hasNext) {
    throw StateError('iterator contract failed');
  }

  print('iterator=${visited.join(',')}');
}

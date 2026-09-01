/// Canonical Dart Memento example for Genkidama.
///
/// The originator owns snapshot creation and restoration. The caretaker keeps
/// an opaque immutable value and never mutates the originator's internals.
final class MementoState {
  MementoState(this.title, List<String> tags) : tags = List.unmodifiable(tags);

  final String title;
  final List<String> tags;
}

final class MementoDocument {
  MementoDocument(this.title, List<String> tags) : tags = List.of(tags);

  String title;
  List<String> tags;

  MementoState save() => MementoState(title, tags);

  void restore(MementoState snapshot) {
    title = snapshot.title;
    tags = List.of(snapshot.tags);
  }
}

void verifyMementoCanonical() {
  final document = MementoDocument('draft', ['pattern']);
  final snapshot = document.save();

  document.title = 'published';
  document.tags.add('edited');

  if (snapshot.title != 'draft' || snapshot.tags.join(',') != 'pattern') {
    throw StateError('snapshot changed with live state');
  }
  if (document.title != 'published' ||
      document.tags.join(',') != 'pattern,edited') {
    throw StateError('live mutation was not observable');
  }

  try {
    snapshot.tags.add('forbidden');
    throw StateError('snapshot tags must be immutable');
  } on UnsupportedError {
    // Expected: the caretaker snapshot is immutable.
  }

  document.restore(snapshot);
  if (document.title != 'draft' || document.tags.join(',') != 'pattern') {
    throw StateError('restore did not recover the snapshot');
  }

  document.tags[0] = 'restored';
  if (snapshot.tags.single != 'pattern') {
    throw StateError('restore aliased the caretaker snapshot');
  }
}

void main() {
  verifyMementoCanonical();
  print('Dart Memento: passed');
}

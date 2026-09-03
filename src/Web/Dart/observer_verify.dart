import 'observer.dart';

void main() {
  if (!observerExamplePasses()) {
    throw StateError('Dart Observer contract failed');
  }

  print('Dart Observer: passed');
}

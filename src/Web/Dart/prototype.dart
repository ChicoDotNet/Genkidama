class ServiceProfile {
  ServiceProfile(this.name, List<String> features)
    : features = List<String>.of(features);

  String name;
  final List<String> features;

  ServiceProfile cloneProfile() => ServiceProfile(name, features);

  String describe() => '$name: ${features.join(',')}';
}

void main() {
  final original = ServiceProfile('orders', ['metrics']);
  final canary = original.cloneProfile()
    ..name = 'orders-canary'
    ..features.add('tracing');

  print('original=${original.describe()}');
  print('clone=${canary.describe()}');
}

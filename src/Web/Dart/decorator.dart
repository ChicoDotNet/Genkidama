typedef Render = String Function();

String plain() {
  return 'alert';
}

Render audit(Render inner) {
  String audited() {
    return 'audit(${inner()})';
  }

  return audited;
}

Render encrypt(Render inner) {
  String encrypted() {
    return 'enc(${inner()})';
  }

  return encrypted;
}

void main() {
  final Render base = plain;
  final audited = audit(base);
  final encrypted = encrypt(base);
  final stacked = audit(encrypt(base));

  print('base=${base()}');
  print('audit=${audited()}');
  print('encrypted=${encrypted()}');
  print('stacked=${stacked()}');
}

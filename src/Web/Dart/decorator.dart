typedef Render = String Function();

String plain() => 'alert';

Render audit(Render inner) => () => 'audit(${inner()})';
Render encrypt(Render inner) => () => 'enc(${inner()})';

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

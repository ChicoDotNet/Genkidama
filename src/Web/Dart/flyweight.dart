class TextStyle {
  const TextStyle(this.font, this.size, this.color);

  final String font;
  final int size;
  final String color;
}

class StyleFactory {
  final Map<String, TextStyle> _pool = <String, TextStyle>{};

  TextStyle get(String font, int size, String color) {
    final key = '$font|$size|$color';
    return _pool.putIfAbsent(key, () => TextStyle(font, size, color));
  }

  int get count => _pool.length;
}

void main() {
  final factory = StyleFactory();
  final red1 = factory.get('Inter', 12, 'red');
  final red2 = factory.get('Inter', 12, 'red');
  final blue = factory.get('Inter', 12, 'blue');
  assert(blue.color == 'blue');
  print('styles=${factory.count};shared=${identical(red1, red2) ? 'true' : 'false'};text=ABC');
}

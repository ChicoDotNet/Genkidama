import groovy.transform.Immutable

@Immutable
class TextStyle {
    String font
    int size
    String color
}

class StyleFactory {
    private final Map<String, TextStyle> pool = [:]

    TextStyle get(String font, int size, String color) {
        String key = "${font}|${size}|${color}"
        pool.computeIfAbsent(key) { new TextStyle(font, size, color) }
    }

    int size() { pool.size() }
}

def styles = new StyleFactory()
def red1 = styles.get('Inter', 12, 'red')
def red2 = styles.get('Inter', 12, 'red')
def blue = styles.get('Inter', 12, 'blue')
assert blue.color == 'blue'
println "styles=${styles.size()};shared=${red1.is(red2)};text=ABC"

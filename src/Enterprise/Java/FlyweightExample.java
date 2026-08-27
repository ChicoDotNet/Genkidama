import java.util.*;

public class FlyweightExample {
  record TextStyle(String font, int size, String color) {}
  static final class StyleFactory {
    private final Map<String, TextStyle> styles = new HashMap<>();
    TextStyle get(String font, int size, String color) {
      return styles.computeIfAbsent(font + "|" + size + "|" + color, k -> new TextStyle(font, size, color));
    }
    int count() { return styles.size(); }
  }
  record Glyph(char character, int position, TextStyle style) {}
  public static void main(String[] args) {
    var f = new StyleFactory(); var r1=f.get("Inter",12,"red"); var r2=f.get("Inter",12,"red"); var b=f.get("Inter",12,"blue");
    var g=List.of(new Glyph('A',1,r1),new Glyph('B',2,r2),new Glyph('C',3,b));
    System.out.println("styles="+f.count()+";shared="+(g.get(0).style()==g.get(1).style())+";text=ABC");
  }
}

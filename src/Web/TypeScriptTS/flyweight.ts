type TextStyle = Readonly<{ font: string; size: number; color: string }>;

class StyleFactory {
  private readonly styles = new Map<string, TextStyle>();
  get(font: string, size: number, color: string): TextStyle {
    const key = `${font}|${size}|${color}`;
    let style = this.styles.get(key);
    if (!style) {
      style = Object.freeze({ font, size, color });
      this.styles.set(key, style);
    }
    return style;
  }
  get count(): number { return this.styles.size; }
}

const factory = new StyleFactory();
const red1 = factory.get("Inter", 12, "red");
const red2 = factory.get("Inter", 12, "red");
const blue = factory.get("Inter", 12, "blue");
const glyphs = [{ c: "A", pos: 1, style: red1 }, { c: "B", pos: 2, style: red2 }, { c: "C", pos: 3, style: blue }];
console.log(`styles=${factory.count};shared=${glyphs[0].style === glyphs[1].style};text=${glyphs.map(g => g.c).join("")}`);

from dataclasses import dataclass

@dataclass(frozen=True)
class TextStyle:
    font: str
    size: int
    color: str

class StyleFactory:
    def __init__(self):
        self._styles = {}
    def get(self, font, size, color):
        key = (font, size, color)
        if key not in self._styles:
            self._styles[key] = TextStyle(*key)
        return self._styles[key]
    @property
    def count(self):
        return len(self._styles)

factory = StyleFactory()
red1 = factory.get("Inter", 12, "red")
red2 = factory.get("Inter", 12, "red")
blue = factory.get("Inter", 12, "blue")
glyphs = [("A", 1, red1), ("B", 2, red2), ("C", 3, blue)]
print(f"styles={factory.count};shared={str(glyphs[0][2] is glyphs[1][2]).lower()};text={''.join(g[0] for g in glyphs)}")

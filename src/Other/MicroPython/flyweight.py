class TextStyle:
    def __init__(self, font, size, color):
        self.font = font
        self.size = size
        self.color = color


class StyleFactory:
    def __init__(self):
        self.pool = {}

    def get(self, font, size, color):
        key = (font, size, color)
        if key not in self.pool:
            self.pool[key] = TextStyle(font, size, color)
        return self.pool[key]


factory = StyleFactory()
red1 = factory.get("Inter", 12, "red")
red2 = factory.get("Inter", 12, "red")
blue = factory.get("Inter", 12, "blue")
assert blue is not red1
print("styles={};shared={};text=ABC".format(
    len(factory.pool), "true" if red1 is red2 else "false"))

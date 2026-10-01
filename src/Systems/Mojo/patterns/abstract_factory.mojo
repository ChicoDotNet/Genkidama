struct ThemeFactory:
    var dark: Bool

    def __init__(out self, dark: Bool):
        self.dark = dark

    def button(self) -> Int:
        if self.dark:
            return 10
        return 20

    def checkbox(self) -> Int:
        if self.dark:
            return 11
        return 21


def run() -> Bool:
    var dark = ThemeFactory(True)
    var light = ThemeFactory(False)
    return dark.button() == 10 and dark.checkbox() == 11 and light.button() == 20 and light.checkbox() == 21

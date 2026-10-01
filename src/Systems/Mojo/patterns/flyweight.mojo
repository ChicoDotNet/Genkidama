def style_for(style_key: Int) -> Int:
    # The intrinsic style is shared; glyph position stays external.
    return style_key * 10


def render(style: Int, position: Int) -> Int:
    return style + position


def run() -> Bool:
    var shared_a = style_for(7)
    var shared_b = style_for(7)
    return shared_a == shared_b and render(shared_a, 1) != render(shared_b, 2)

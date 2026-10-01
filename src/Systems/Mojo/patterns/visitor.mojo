struct Shape:
    var kind: Int
    var size: Int

    def __init__(out self, kind: Int, size: Int):
        self.kind = kind
        self.size = size


def area_visitor(shape: Shape) -> Int:
    if shape.kind == 1:
        return shape.size * shape.size
    return shape.size * 2


def label_visitor(shape: Shape) -> Int:
    return shape.kind * 100 + shape.size


def run() -> Bool:
    var square = Shape(1, 4)
    var line = Shape(2, 4)
    return area_visitor(square) == 16 and area_visitor(line) == 8 and label_visitor(square) == 104

struct Product:
    var kind: Int

    def __init__(out self, kind: Int):
        self.kind = kind

    def operate(self) -> Int:
        return self.kind * 10


def create_product(kind: Int) -> Product:
    return Product(kind)


def run() -> Bool:
    var product = create_product(7)
    return product.operate() == 70

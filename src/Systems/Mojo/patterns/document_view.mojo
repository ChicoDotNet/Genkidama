struct Document:
    var value: Int

    def __init__(out self, value: Int):
        self.value = value


def detail_view(document: Document) -> Int:
    return document.value * 10


def summary_view(document: Document) -> Int:
    return document.value


def run() -> Bool:
    var document = Document(7)
    return detail_view(document) == 70 and summary_view(document) == 7

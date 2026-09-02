class Document:
    def __init__(self, title, tags):
        self.title = title
        self.tags = list(tags)

    def save(self):
        return (self.title, tuple(self.tags))

    def restore(self, snapshot):
        self.title = snapshot[0]
        self.tags = list(snapshot[1])


def verify_memento_canonical():
    originator = Document("draft", ["pattern"])
    caretaker_snapshot = originator.save()

    originator.title = "published"
    originator.tags.append("edited")
    assert caretaker_snapshot == ("draft", ("pattern",))

    originator.restore(caretaker_snapshot)
    assert originator.title == "draft"
    assert originator.tags == ["pattern"]

    originator.title = "restored-edit"
    originator.tags.append("restored")
    assert caretaker_snapshot == ("draft", ("pattern",))

    print("MicroPython Memento: passed")


verify_memento_canonical()

struct ActiveRecord:
    var id: Int
    var value: Int

    def __init__(out self, id: Int, value: Int):
        self.id = id
        self.value = value

    def save(self) -> Int:
        return self.id * 100 + self.value


def run() -> Bool:
    var record = ActiveRecord(4, 7)
    return record.save() == 407

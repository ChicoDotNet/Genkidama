struct Entity:
    var id: Int
    var value: Int

    def __init__(out self, id: Int, value: Int):
        self.id = id
        self.value = value


def find_by_id(id: Int) -> Entity:
    return Entity(id, id * 10)


def run() -> Bool:
    var entity = find_by_id(4)
    return entity.id == 4 and entity.value == 40

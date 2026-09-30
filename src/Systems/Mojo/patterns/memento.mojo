struct Editor:
    var value: Int

    def __init__(out self, value: Int):
        self.value = value

    def snapshot(self) -> Int:
        return self.value

    def edit(mut self, value: Int):
        self.value = value

    def restore(mut self, snapshot: Int):
        self.value = snapshot


def run() -> Bool:
    var editor = Editor(7)
    var saved = editor.snapshot()
    editor.edit(99)
    editor.restore(saved)
    return editor.value == 7

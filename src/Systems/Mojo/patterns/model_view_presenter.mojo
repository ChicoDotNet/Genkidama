struct Presenter:
    var model: Int
    var view_value: Int

    def __init__(out self, model: Int):
        self.model = model
        self.view_value = 0

    def present(mut self):
        self.view_value = self.model * 10


def run() -> Bool:
    var presenter = Presenter(6)
    presenter.present()
    return presenter.view_value == 60

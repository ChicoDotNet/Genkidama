struct ReportBuilder:
    var header: Int
    var body: Int
    var footer: Int

    def __init__(out self):
        self.header = 0
        self.body = 0
        self.footer = 0

    def with_header(mut self, value: Int):
        self.header = value

    def with_body(mut self, value: Int):
        self.body = value

    def with_footer(mut self, value: Int):
        self.footer = value

    def build(self) -> Int:
        return self.header * 100 + self.body * 10 + self.footer


def run() -> Bool:
    var builder = ReportBuilder()
    builder.with_header(1)
    builder.with_body(2)
    builder.with_footer(3)
    return builder.build() == 123

struct Pipeline:
    var queued: Int
    var processed: Int

    def __init__(out self):
        self.queued = 0
        self.processed = 0

    def async_receive(mut self, value: Int):
        self.queued += value

    def sync_process(mut self):
        self.processed += self.queued * 2
        self.queued = 0


def run() -> Bool:
    var pipeline = Pipeline()
    pipeline.async_receive(2)
    pipeline.async_receive(3)
    pipeline.sync_process()
    return pipeline.processed == 10 and pipeline.queued == 0

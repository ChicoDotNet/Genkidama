"""Canonical Iterator example for Python.

The aggregate owns its traversal policy and exposes Python's native iterator
protocol without leaking an index or its internal representation.
"""


class Countdown:
    def __init__(self, start: int) -> None:
        if start < 0:
            raise ValueError("start must be non-negative")
        self._start = start

    def __iter__(self):
        current = self._start
        while current > 0:
            yield current
            current -= 1


def run() -> bool:
    countdown = Countdown(3)
    first_pass = list(countdown)
    second_pass = list(countdown)
    return first_pass == [3, 2, 1] and second_pass == first_pass


if __name__ == "__main__":
    assert run()
    print("iterator=3,2,1")

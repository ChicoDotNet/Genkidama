def make_iterator(values):
    index = 0

    def next_value():
        nonlocal index
        if index >= len(values):
            return None
        current = values[index]
        index += 1
        return current

    return next_value


next_value = make_iterator((10, 20, 30))
visited = []
while True:
    value = next_value()
    if value is None:
        break
    visited.append(value)

if visited != [10, 20, 30] or next_value() is not None:
    raise RuntimeError("Iterator contract failed")
print("iterator=10,20,30")

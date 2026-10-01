def leaf_size(value: Int) -> Int:
    return value


def composite_size(left: Int, right: Int) -> Int:
    # Leaves and composites expose the same size result to their parent.
    return left + right


def run() -> Bool:
    var docs = composite_size(leaf_size(2), leaf_size(3))
    var root = composite_size(docs, leaf_size(5))
    return root == 10

class FileLeaf:
    def __init__(self, size):
        self._size = size

    def size(self):
        return self._size


class FolderComposite:
    def __init__(self, children):
        self._children = list(children)

    def size(self):
        total = 0
        for child in self._children:
            total += child.size()
        return total


readme = FileLeaf(2)
docs = FolderComposite((FileLeaf(3), FileLeaf(5)))
root = FolderComposite((readme, docs))

print("leaf={}".format(readme.size()))
print("docs={}".format(docs.size()))
print("root={}".format(root.size()))

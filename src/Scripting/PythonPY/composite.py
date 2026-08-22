from typing import Protocol


class Component(Protocol):
    def size(self) -> int: ...


class FileLeaf:
    def __init__(self, size: int) -> None:
        self._size = size

    def size(self) -> int:
        return self._size


class FolderComposite:
    def __init__(self, *children: Component) -> None:
        self._children = list(children)

    def size(self) -> int:
        return sum(child.size() for child in self._children)


readme: Component = FileLeaf(2)
docs: Component = FolderComposite(FileLeaf(3), FileLeaf(5))
root: Component = FolderComposite(readme, docs)

print(f"leaf={readme.size()}")
print(f"docs={docs.size()}")
print(f"root={root.size()}")

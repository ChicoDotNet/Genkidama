interface Node {
    int size()
}

final class FileLeaf implements Node {
    final int bytes

    FileLeaf(int bytes) {
        this.bytes = bytes
    }

    int size() { bytes }
}

final class FolderComposite implements Node {
    final List<Node> children

    FolderComposite(List<Node> children) {
        this.children = children
    }

    int size() { children.sum { it.size() } as int }
}

def readme = new FileLeaf(2)
def docs = new FolderComposite([new FileLeaf(3), new FileLeaf(5)])
def root = new FolderComposite([readme, docs])

println "leaf=${readme.size()}"
println "docs=${docs.size()}"
println "root=${root.size()}"

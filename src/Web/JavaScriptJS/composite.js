class FileLeaf {
  constructor(bytes) {
    this.bytes = bytes;
  }

  size() {
    return this.bytes;
  }
}

class FolderComposite {
  constructor(children) {
    this.children = children;
  }

  size() {
    return this.children.reduce((total, child) => total + child.size(), 0);
  }
}

const readme = new FileLeaf(2);
const docs = new FolderComposite([new FileLeaf(3), new FileLeaf(5)]);
const root = new FolderComposite([readme, docs]);

console.log(`leaf=${readme.size()}`);
console.log(`docs=${docs.size()}`);
console.log(`root=${root.size()}`);

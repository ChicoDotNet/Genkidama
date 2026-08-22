interface Component {
  size(): number;
}

class FileLeaf implements Component {
  constructor(private readonly bytes: number) {}
  size(): number { return this.bytes; }
}

class FolderComposite implements Component {
  constructor(private readonly children: Component[]) {}
  size(): number { return this.children.reduce((total, child) => total + child.size(), 0); }
}

const readme: Component = new FileLeaf(2);
const docs: Component = new FolderComposite([new FileLeaf(3), new FileLeaf(5)]);
const root: Component = new FolderComposite([readme, docs]);

console.log(`leaf=${readme.size()}`);
console.log(`docs=${docs.size()}`);
console.log(`root=${root.size()}`);

protocol Component {
    func size() -> Int
}

struct FileLeaf: Component {
    let bytes: Int
    func size() -> Int { bytes }
}

struct FolderComposite: Component {
    let children: [any Component]
    func size() -> Int { children.reduce(0) { $0 + $1.size() } }
}

let readme: any Component = FileLeaf(bytes: 2)
let docs: any Component = FolderComposite(children: [FileLeaf(bytes: 3), FileLeaf(bytes: 5)])
let root: any Component = FolderComposite(children: [readme, docs])

print("leaf=\(readme.size())")
print("docs=\(docs.size())")
print("root=\(root.size())")

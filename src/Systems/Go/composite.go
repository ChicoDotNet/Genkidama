package main

import "fmt"

type Component interface {
	Size() int
}

type FileLeaf struct {
	bytes int
}

func (f FileLeaf) Size() int { return f.bytes }

type FolderComposite struct {
	children []Component
}

func (f FolderComposite) Size() int {
	total := 0
	for _, child := range f.children {
		total += child.Size()
	}
	return total
}

func main() {
	var readme Component = FileLeaf{bytes: 2}
	var docs Component = FolderComposite{children: []Component{FileLeaf{bytes: 3}, FileLeaf{bytes: 5}}}
	var root Component = FolderComposite{children: []Component{readme, docs}}

	fmt.Printf("leaf=%d\n", readme.Size())
	fmt.Printf("docs=%d\n", docs.Size())
	fmt.Printf("root=%d\n", root.Size())
}
